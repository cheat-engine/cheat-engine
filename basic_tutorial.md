**The tutorial of cheat engine: (by Kerui Liu)**

Since the tutorial web page of this project is down([https://forum.cheatengine.org/viewtopic.php?t=26540](https://forum.cheatengine.org/viewtopic.php?t=26540)), this document will give a detailed tutorial of cheat engine (will be called CE in the rest of file) with specific example.

(All demonstrations will be presented with 7.4 version. )

**Interface:**

![](RackMultipart20220912-1-3q7rvp_html_56298800cdb3e1a8.png) ![](RackMultipart20220912-1-3q7rvp_html_a568d0bcb90d0a9b.png) The interface of the CE would like this.

The icon in the left top corner is where you should start interacting. It will lead you to a process list that you are currently running. By selecting the program or game you want to modify, you can start to "hack" it. In the example, the Minecraft will be taken as example.

**To modify the existing data:**

![](RackMultipart20220912-1-3q7rvp_html_4f8400e595e49dfa.png)

![](RackMultipart20220912-1-3q7rvp_html_6547df26f8958a31.png)

![](RackMultipart20220912-1-3q7rvp_html_c5d876fafe41df0f.png)Assuming I want to modify the number of dirt, the first step is simply putting the number of dirt cube in the value section, then do the first scan. The CE will scan all the memory addresses of the application, then list all addresses that the value is 10. The output of first scan usually will give a large number, it's impossible we test it one by one, so users may need to do multiple scans by changing the number of dirt cube.

![](RackMultipart20220912-1-3q7rvp_html_93a28357b871b572.png)

After a few times scans, there only a few addresses should be left. There are various situations that may happen:

1. Please noted that sometimes the application will not just store the data of an item in one address, it could be stored in multiple addresses for various purposes. So, the situation that multiple address's values will stay the same even if the user is changing the value will occur. The solution is by examining the address one by one.

In most applications, programmers will tend to plan a certain section of memory for certain purposes, which means the addresses that are used to store the number of items will have a similar pattern, for example, the address of the poppy is BA7CDA8C, accordingly the user can assume that the address 9F50D59C is the address of dirt cube.

1. The second situation is shown in the picture and it's what the user will commonly see. In this situation, we can see there are three addresses left after scans. However, two of them are in red font. This is because they are in a dynamic motion, as a result of it the value will keep changing. So, when the user is doing the scan, they should always wait a few seconds to see if any of them is dynamic (yes, they will not instantly turn red after a scan) before they start the new scan.

Finally, simply right click on the address and select "change value of selected address" option to modify it.

**How you implement testing before him change the value of address if you are not sure if it's correct address?**

An easy way to test if you are locating correct address is by examining the memory region. Suppose we don't know

![](RackMultipart20220912-1-3q7rvp_html_e405ad4bb2c1e891.png) the address of poppy, let's right click the address BA7CDA8C and select "browse this memory region". A memory viewer will pop out and automatically locate to the targeted address. The first section in the address is 5A, which is the hexadecimal of 90, and the number of poppies is 90. The address is proven that is used for poppies

![](RackMultipart20220912-1-3q7rvp_html_455e84c7e4c5eb2.png)

**How to do magic out of nothing?**

This is not recommended because it may cause the application crash, and usually hard to implement.

To get something you don't have, you need to know the "ID" of the item, for example, the "ID" of poppy is 40 (in hex:28), which you have rare chances to find them on the internet(Even you do, there's a highly chance the game versions are not same). Then, you need to empty your backpack (or any form of storage) in the game to avoid crash (if you generate a new item in a address where already has a item, the game crashes). Finally, entering the ID and wish number in the correct section.

**Author: Kerui\_Liu**
