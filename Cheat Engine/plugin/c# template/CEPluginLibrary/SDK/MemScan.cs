using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CESDK
{
    //Not much of an SDK but more an example of how to wrap the exposed classes by CE into C# classes.  Learn from this and implement the other features you like

    public enum ScanOptions
    {
        soUnknownValue = 0,
        soExactValue = 1,
        soValueBetween = 2,
        soBiggerThan = 3,
        soSmallerThan = 4,
        soIncreasedValue = 5,
        soIncreasedValueBy = 6,
        soDecreasedValue = 7,
        soDecreasedValueBy = 8,
        soChanged = 9,
        soUnchanged = 10
    }

    public enum VarTypes
    {
        vtByte = 0,
        vtWord = 1,
        vtDword = 2,
        vtQword = 3,
        vtSingle = 4,
        vtDouble = 5,
        vtString = 6,
        vtUnicodeString = 7, //--Only used by autoguess
        vtWideString = 7,
        vtByteArray = 8,
        vtBinary = 9,
        vtAll = 10,
        vtAutoAssembler = 11,
        vtPointer = 12, //--Only used by autoguess and structures
        vtCustom = 13,
        vtGrouped = 14
    }

    public enum RoundingTypes
    {
        rtRounded = 0,
        rtExtremerounded = 1,
        rtTruncated = 2
    }

    public enum FastScanMethods
    {
        fsmNotAligned = 0,
        fsmAligned = 1,
        fsmLastDigits = 2
    }

    public class ScanParameters
    {
        public string Value;
        public string Value2; //needed for between
        public ScanOptions ScanOption;
        public VarTypes VarType;
        public RoundingTypes RoundingType;
        public UInt64 StartAddress;
        public UInt64 StopAddress;
        public string ProtectionFlags;
        public FastScanMethods AlignmentType;
        public string AlignmentValue;
        public bool isHexadecimalInput;
        public bool isUTF16Scan;
        public bool isCaseSensitive; //for strings
        public bool isPercentageScan; //next scan

        public ScanParameters()
        {
            //default config
            ScanOption = ScanOptions.soExactValue;
            VarType = VarTypes.vtDword;
            RoundingType = RoundingTypes.rtExtremerounded;
            StartAddress = 0;
            StopAddress = (UInt64)(Int64.MaxValue);
            ProtectionFlags = "+W-C"; //wirable, not copy-on-write, and ignore execute
            AlignmentType = FastScanMethods.fsmAligned;
            AlignmentValue = "4";
            isHexadecimalInput = false;
            isUTF16Scan = false;
            isCaseSensitive = false;
            isPercentageScan = false;
        }
    }

    class MemScan : CEObjectWrapper
    {
        private bool scanStarted;

        public delegate void dScanDone(object sender);
        public delegate void dGUIUpdate(object sender, UInt64 TotalAddressesToScan, UInt64 CurrentlyScanned, UInt64 ResultsFound);

        public dScanDone OnScanDone;
        public dGUIUpdate OnGuiUpdate;


        public void WaitTillDone()
        {
            lua.PushCEObject(CEObject);
            lua.PushString("waitTillDone");
            lua.GetTable(-2);

            if (lua.IsFunction(-1) == false) throw new System.ApplicationException("memscan object without a waitTillDone method");

            lua.PCall(0, 0);
            lua.SetTop(0);
        }

        public void Scan(ScanParameters p)
        {
            try
            {
                lua.PushCEObject(CEObject);


                if (scanStarted == false)
                {
                    //first scan
                    lua.PushString("firstScan");
                    lua.GetTable(-2);
                    
                    if (lua.IsFunction(-1)==false) throw new System.ApplicationException("memscan object without a firstScan method");

                    lua.PushInteger((long)p.ScanOption);
                    lua.PushInteger((long)p.VarType);
                    lua.PushInteger((long)p.RoundingType);
                    lua.PushString(p.Value);
                    lua.PushString(p.Value2);
                    lua.PushInteger((long)p.StartAddress);
                    lua.PushInteger((long)p.StopAddress);
                    lua.PushString(p.ProtectionFlags);
                    lua.PushInteger((long)p.AlignmentType);
                    lua.PushString(p.AlignmentValue);
                    lua.PushBoolean(p.isHexadecimalInput);
                    lua.PushBoolean(true); //isnotabinarystring
                    lua.PushBoolean(p.isUTF16Scan);
                    lua.PushBoolean(p.isCaseSensitive);
                    lua.PCall(14, 0);

                    scanStarted = true;
                }
                else
                {
                    //next scan
                    lua.PushString("nextScan");
                    lua.GetTable(-2);

                    //nextScan(scanoption, roundingtype, input1,input2, isHexadecimalInput, isNotABinaryString, isunicodescan, iscasesensitive, ispercentagescan, savedresultname OPTIONAL);

                    if (lua.IsFunction(-1) == false) throw new System.ApplicationException("memscan object without a nextScan method");

                    lua.PushInteger((long)p.ScanOption);
                    lua.PushInteger((long)p.RoundingType);
                    lua.PushString(p.Value);
                    lua.PushString(p.Value2);
                    lua.PushBoolean(p.isHexadecimalInput);
                    lua.PushBoolean(true);
                    lua.PushBoolean(p.isUTF16Scan);
                    lua.PushBoolean(p.isCaseSensitive);
                    lua.PushBoolean(p.isPercentageScan);
                    lua.PCall(9, 0);
                }
            }
            finally
            {
                lua.SetTop(0);
            }

        }

        public void Reset()
        {
            //new scan
            lua.PushCEObject(CEObject);
            lua.PushString("newScan");
            lua.GetTable(-2);

            if (lua.IsFunction(-1) == false) throw new System.ApplicationException("memscan object without a newScan method");

            lua.PCall(0, 0);
            lua.SetTop(0);

            scanStarted = false;
        }

        private int LScanDone(IntPtr L)
        {
            //function(memscan) - called when the scan has finished
            if (OnScanDone != null)
                OnScanDone(this);

            return 0;
        }

        private int LGuiUpdate(IntPtr L)
        {
            //function(memscan, TotalAddressesToScan, CurrentlyScanned, ResultsFound) - Called during the scan so you can update the interface if needed
            if (OnGuiUpdate != null)
            {
                if (lua.GetTop()>=4)
                    OnGuiUpdate(this, (UInt64)lua.ToInteger(2), (UInt64)lua.ToInteger(3), (UInt64)lua.ToInteger(4));
            }

            return 0;
        }

        CESDKLua.LuaCall dLScanDone; //prevent garbage collection
        CESDKLua.LuaCall dLGuiUpdate;


        /// <summary>
        /// Gets a MemScan object from CE
        /// </summary>
        public MemScan()
        {
            try
            {
                lua.GetGlobal("createMemScan");
                if (lua.IsNil(-1))
                    throw new System.ApplicationException("You have no createFoundList (WTF)");
                               
                lua.PCall(0, 1);

                if (lua.IsCEObject(-1))
                {
                    CEObject = lua.ToCEObject(-1);

                    //setup what happens when the scan is done
                    dLScanDone = LScanDone;
                    dLGuiUpdate = LGuiUpdate;

                    lua.PushString("OnScanDone");
                    lua.PushFunction(dLScanDone);
                    lua.SetTable(-3);

                    lua.PushString("OnGuiUpdate");
                    lua.PushFunction(dLGuiUpdate);
                    lua.SetTable(-3);                       
                        
                }
                else
                    throw new System.ApplicationException("No idea what it returned");                
                
            }
            finally
            {
                lua.SetTop(0);
            }
        }
    }
}
