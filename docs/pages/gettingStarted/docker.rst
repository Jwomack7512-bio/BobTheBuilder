============================
Pull Docker Image
============================

The application has been created as a docker image for user convience if they
wish to run the app locally without installing R or worrying about other 
possible dependencies. 

Download Docker 
-----------------------
In this section, we will download docker to our computer. 
This will install the desktop app but what we really need is the docker 
terminal commands to be added. 
If you have a working version of Docker on your system, 
skip to the next section.


#. Go to http://www.docker.com/products/docker-desktop
#. Download docker for your respective system. 
   This tutorial follows the windows download.

   .. container:: bordergrey
        
        .. image:: images/docker/download_page.png

#. Go to you downloads folder and begin the system install of docker. 
   This download will take a few minutes.
#. A configuration popup will appear. Keep "Install required Windows components
   for WSL 2" checked.  You can uncheck the desktop shortcut option if you want.

   .. container:: bordergrey
        
        .. image:: images/docker/download_options.png

#. Once download is complete a message will appear to restart your system.
   Perform computer restart. 
#. On restart you may get a notification that "WSL 2 installation is 
   incomplete". If this is the case you will need to install this update or 
   the docker image will not run. Follow the link on the popup, it takes you 
   to the following link: 
   https://docs.microsoft.com/en-us/windows/wsl/install-manual#step-4--
   -download-the-linux-kernel-update-package}{https://docs.microsoft.com/en-us
   /windows/wsl/install-manual#step-4---download-the-linux-kernel-update-
   package. Download and install the kernel from this link.

   .. container:: bordergrey
    
    .. image:: images/docker/download_wsl_popup.png

#. Restart computer again and open docker program. 
   It should be fully installed and working now.
   A working program has a green color in the bottom left corner and lacks a 
   huge error message in the center of the application.

   .. container:: bordergrey
    
    .. image:: images/docker/working_docker.png

Note: A Docker account does not need to be made in order to pull and run this
program. 

Pull/Run From Docker
-----------------------


Loading Application After Pull
--------------------------------

