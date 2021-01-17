# Modern-Fortran syntax

Modern-Fortran is a [Sublime Text 3](https://www.sublimetext.com/) language syntax for highlighting of Fortran code. It highlights modern Fortran code, starting with the Fortran 90 and incorporating many of the features introduced in Fortran 2003, 2008, and 2018. 

## Installation via Package Control
The easiest way to get the syntax is to install the package ModernFortran with Package Control. 

After installation, you can enable the syntax by opening a Fortran source file and typing `ctrl+shift+p` (`command+shift+p` on Mac OS) followed by `Set syntax: Modern-Fortran`.

## Manual installation from source
The syntax can also be installed manually.
Open the terminal and navigate to the folder where packages are installed:
```shell
cd /home/username/.config/sublime-text-3/Packages # Default location on Ubuntu
```
The location on your computer can be found via `Preferences -> Browse packages...`. Next, clone the repository:
```shell
git clone https://github.com/eirik-kjonstad/modern-fortran-syntax.git
```
Alternatively, you can download the source code as a `.zip` and extract the contents in the Packages folder.

## Notice bugs, missing features, or other problems?
Do not hesitate to post an issue in the `issues` tab on the repository. If you wish to contribute, we also welcome pull requests. Please fork the repository and open a pull request with your modifications.
