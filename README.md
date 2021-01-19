![Syntax Tests](https://github.com/eirik-kjonstad/modern-fortran-syntax/workflows/Syntax%20Tests/badge.svg)

# Modern-Fortran syntax

Modern-Fortran is a [Sublime Text 3](https://www.sublimetext.com/) language syntax for highlighting of Fortran code. It highlights modern Fortran code (Fortran 90 and newer) and incorporates features introduced in Fortran 2003, 2008, and 2018. 

## Installation via Package Control
The easiest way to get the syntax is to install the package *ModernFortran* with [Package Control](https://packagecontrol.io/). 

After installation, you can enable the syntax with `ctrl+shift+p` (`command+shift+p` on Mac OS) followed by `Set syntax: Modern-Fortran`.

## Manual installation from source
The syntax can also be installed manually.
Open the terminal and navigate to where packages are installed:
```shell
cd /home/username/.config/sublime-text-3/Packages # Default location on Ubuntu
```
The location on your computer can be found via `Preferences -> Browse packages...`. Next, clone the repository:
```shell
git clone https://github.com/eirik-kjonstad/modern-fortran-syntax.git
```
Alternatively, you can download the source code as a `.zip` and extract the contents in the Packages folder.

## Notice bugs, missing features, or other problems?
Do not hesitate to post an issue in the `issues` tab on the repository! If you wish to contribute, we also welcome pull requests. Please fork the repository and open a pull request with your suggested modifications.
