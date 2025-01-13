# Build Cheat Engine with **Lazarus**

This guide will help you build Cheat Engine using Lazarus, the open-source Delphi-like IDE.

## Prerequisites

1. **Install Lazarus**  
   Download and install Lazarus from the official website:  
   [Lazarus Windows 64-bit](https://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2064%20bits/).

2. **Install the necessary dependencies**  
   Ensure you have the following dependencies installed:
   - **Free Pascal Compiler (FPC)**
   - **Lazarus IDE** (which will install FPC automatically)
   
   You can download FPC from [Free Pascal](https://www.freepascal.org/download.html).

3. **Download Cheat Engine Source Code**  
   Clone the Cheat Engine repository or download the latest zip release from GitHub:
  
  https://github.com/moxiu6/cheat-engine

5. **Set up the environment**  
   Make sure you have set up the required libraries in Lazarus:
   - Open Lazarus IDE.
   - Go to `Tools` → `Options`.
   - Select `Environment` → `Files and Directories`.
   - Make sure to set the paths for **FPC** and **Lazarus** if they aren't automatically configured.

## Build Steps

### Step 1: Open Cheat Engine Project

1. Open the **Cheat Engine** project in Lazarus:
   - Navigate to the downloaded **Cheat Engine source** folder.
   - Open `cheatengine.lpi` file with Lazarus.

### Step 2: Configure Project Settings

1. Check the project settings to ensure the correct target architecture (32-bit or 64-bit) and platform (Windows):
   - Go to `Project` → `Project Options`.
   - Choose the appropriate **Target CPU** and **Target OS** (Windows).
   
2. Configure **Lazarus IDE settings**:
   - Open `Project` → `Compiler Options`.
   - Set the options for the build, ensuring it matches the needed settings for Cheat Engine.

### Step 3: Build the Project

1. In Lazarus IDE, click **Run** or press `F9` to start the build process.
2. Watch the output window for any errors or warnings that might need attention.
3. If successful, the **Cheat Engine executable** will be built and located in the project's `bin` folder.

### Step 4: Testing the Build

1. After the build completes, navigate to the `bin` directory inside the Cheat Engine source folder.
2. Run **cheatengine.exe** to test if it launches and operates as expected.
