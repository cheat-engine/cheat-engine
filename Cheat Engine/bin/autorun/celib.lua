--compiles some handy utilities into the target process you can call using c (or assembler if you so wish)
--add #include <celib.h> to your c-code
--
--functions:
--csEnter/csLeave:  Low level criticial section implementation that does an active wait (more cpu intensive, but safe enough to use in an environment with no other API access)


function injectCEHelperLib()
  if getAddressSafe('csenter')==nil then
    local s
    if targetIsX86() then  
      s=[[
alloc(cespinlock,32)
cespinlock:  
]]
    
      --write the code to bit test and set the locked variable
      if targetIs64Bit() then
        if getABI()==0 then --windows uses rcx as first param
          s=s..[[
lock bts [rcx],0
]]      
        else
          --rdi contains the first param
          s=s..[[
lock bts [rdi],0
]]        
        end
      else
        --both use the push param method
        s=s..[[
mov eax,[esp+4]
lock bts [eax],0
]]     
      end
    
      s=s..[[
jc cespinlock_wait
ret

cespinlock_wait:
pause
jmp cespinlock 
]]
    else      
      error("todo: implement a spinlock for arm (could halfass it and just call and api)")
    end  
      
    s=s..[[
{$c} 
    #include <celib.h>
    extern void cespinlock(int *lock);
#ifdef _WIN32
    extern int getCurrentThreadID();
#else
    extern int gettid();
    int getCurrentThreadID()
    {
      return gettid();
    }
#endif
    
    void csenter(cecs *cs)
    {
      if ((cs->locked) && (cs->threadid==getCurrentThreadID()))
      {
        cs->lockcount++;
        return;
      }

      cespinlock(&cs->locked);
      cs->threadid=getCurrentThreadID();
      cs->lockcount++;
    }

    void csleave(cecs *cs)
    {
      cs->lockcount--;
      if (cs->lockcount==0)
      {
        cs->threadid=0;
        cs->locked=0;
      }

    }    
{$asm}
]]

    local r,r2=autoAssemble(s)
    if r then
      r2.ccodesymbols.name='CELib'
      return r
    else
      return false, r2
    end    
  else
    return true  
  end
end