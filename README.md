# Modern Fortran syntax

The `modern-fortran-syntax` package is a [Sublime Text 3](https://www.sublimetext.com/) language syntax plugin for highlighting of modern Fortran code. It incorporates features introduced in recent Fortran language standards (2003, 2008, 2018). 

## Installation via Package Control
The easiest way to get the syntax is to install the package ModernFortran with Package Control. 

After installation, you can enable the syntax by opening a Fortran source file and typing `ctrl+shift+p` (`command+shift+p` on Mac OS) followed by `Set syntax: Modern-Fortran`.

## Manual installation from source
The syntax can also be installed manually.
Open the terminal and navigate to the folder where Sublime Text packages are installed:
```shell
# Default locations on Mac OS and Ubuntu, respectively
cd /home/username/.config/sublime-text-3/Packages
cd /Users/username/Library/Application\ Support/Sublime\ Text\ 3/Packages
```
The location on your computer can be found by navigating to `Preferences -> Browse packages...` in Sublime Text.

Next, download the repository using `git`:
```shell
git clone https://github.com/eirik-kjonstad/modern-fortran-syntax.git
```
Alternatively, you can download the source code as a `.zip` and extract the contents in the Packages folder.

## Notice bugs, missing features, or other problems?
Do not hesitate to post an issue in the `issues` tab on the repository. If you wish to contribute, we also welcome pull requests. Please fork the repository and open a pull request with your modifications.
