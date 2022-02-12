# roygbyte's Emacs Configuration

## Overview

Switching to Emacs has been life changing. Although it took some pains to get a grip on this ancient editor's nuances, now that my fingers have captured the keystrokes necessary to write code, and my brain is beginning to understand the murky depths of ELips, I finally feel "at one" with my editor. 

This configuration file contains my own modest tweaks to make Emacs my own. It's not much yet, but I expect it will change and grow as I do, moving through my programming journey.

## Appearance

I've always preferred dark user interfaces and themes. Not sure why, except that I find it much easier on my eyes. I started using Emacs with the Zenburn theme, then cycled through a number of Doom themes before ending up on 1337 as a favorite. I decided to personalize the theme after being inspired by other folks' setups, particularily [@elilla's](https://www.reddit.com/r/emacs/comments/k24scc/making_emacs_cute_and_gay_is_hard_work_but_well/) "cute and gay" theme. Anyways, here's a recent screenshot of what I've got:

![Screenshot from 2022-02-12 17-20-56](https://user-images.githubusercontent.com/82218266/153728882-e7588e6d-7a2f-4d24-9870-00adc937d648.png)

For some reason, it was very important for me to have pink in there. Through trial and error, I found that color worked best as a subtle accent. 

# Installation

## Fonts

I am using Iosevka for the default font and Victor Mono Italic for comments and strings. Victor Mono Italic is super cute and quirky and I never thought I'd be into using it. Buuuut seeing its wonky form just brings me so much joy. 

On Ubuntu, fonts should be downloaded and extracted to `/usr/share/fonts`. Afterwards, run `sudo fc-cache` to have the font cache cleared.

- [Iosevka](https://github.com/be5invis/Iosevka) (Super TTC 11.3.0)
- [Victor Mono](https://github.com/rubjo/victor-mono) 
