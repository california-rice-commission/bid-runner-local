# bid-runner-local
Analyze bids for flooding fields on a local machine (or remote desktop)





### Getting large data files
Many of the data files used by this project are too large for regular GitHub storage.  There are three ways to obtain these files.  First, this project uses Git Large File Storage (LFS) to store and manage said files, and you can use this too. A second option is to download the zipped file from GitHub that includes code and data. The final option is to download the zipped 'data' file from PLACEHOLDER and place it on your local machine (assumed location: bid-runner-local/data, configurable in definitions.R).

#### Using Git LFS
1. Install Git LFS if you don't have it (it is installed with GitHub Desktop by default). Navigate to git-lfs.github.com and click Download.
2. Open Git Bash
3. Navigate to this project's directory `cd PATH/TO/bid-runner-local`
4. Add tif files to be tracked `git lfs track "*.tif"`

More information:
https://docs.github.com/en/repositories/working-with-files/managing-large-files/installing-git-large-file-storage
https://docs.github.com/en/repositories/working-with-files/managing-large-files/configuring-git-large-file-storage

#### Downloading archive from 