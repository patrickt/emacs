# emacs

Hi there. This is my emacs setup, assembled over the course of ten years.

![a screenshot](https://github.com/patrickt/emacs/blob/master/ivy-rich-screenshot.png)

In the tradition of the greatest yak shaves, this is pretty well-commented, 
and uses the `use-package` macro for maximum concision and reusability. 
Unlike a lot of Emacs setups out there, it is contained in one and only 
one file, and it should Just Work: drag it into `.emacs.d/init.el` and 
it will install everything it needs to.

This is geared towards using the [railwaycat](https://github.com/railwaycat/homebrew-emacsmacport/releases)
port of Emacs, though it should work fine on other Unix systems. If it doesn't, 
please file a bug. It probably won't work on Windows, but it might? 
