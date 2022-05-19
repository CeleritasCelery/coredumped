+++
title = "Taking org-roam everywhere with logseq"
author = ["Troy Hinckley"]
date = 2021-05-26
draft = false
+++

Update: 2022-05-13

I love [org-roam](https://www.orgroam.com/). It lets me take notes in a way that matches how I think. It makes it easy to recall what I have learned and find connections between ideas. But there has always been one big problem with org-roam: it ties me to the desktop. When I am on the go and all I have is my phone, I don't have access to my notes.

There are some stop gap solutions to try and fix this. [Beorg](https://beorgapp.com/) is iOS app that supports editing org files on mobile. However, it is more focused on task management than note taking. It does not offer text search, has no way to insert "org-roam-links", and does not supported nested directories of org files. [Organice](https://organice.200ok.ch/) is another org mobile solution that can be used as PWA. However, it suffers from the same limitations as beorg, and has a poor editing experience (when you switch away from the app it will close your text box, making it hard to take notes on something you are reading). In the end, I was not satisfied with anything that I found: nothing allowed the same workflow I had on desktop.

Then I happened upon logseq. It is open-source, privacy-centric note taking app inspired by org-mode and [roam-research](https://roamresearch.com/). It has native support for the org mode format. Since both org-roam and logseq are based around roam itself, I can use the same workflow on both apps. It has all the features you would expect of a roam replica, including full text search, page links, inline images/video's, etc. As a bonus, it is fairly easy to make it work with org-roam.


## Configuring logseq {#configuring-logseq}

Logseq has an app for [iOS](https://apps.apple.com/us/app/logseq/id1601013908?platform=ipad) and [Android](https://github.com/logseq/logseq/releases/tag/nightly) (beta). These will use some shared or local storage on your device to access the notes. For me, I store my notes on iCloud Drive, which is available on both my Mac and my iPhone.

Once the org-roam files have been moved to the correct place, we need to setup logseq. First open the settings:

![](/images/logseq-settings.png)
We need set Preferred file format to `Org` and Preferred workflow to `TODO/DOING`.
![](/images/logseq-setting-editor.png)

From there we need update some of the advanced settings to work with org-roam.

{{< figure src="/images/logseq-config-edit.png" >}}

We are going to add 1 line here.
`:org-mode/insert-file-link? true`

{{< figure src="/images/logseq-config-org-file-links.png" >}}


## Configuring org-roam {#configuring-org-roam}

For the emacs-side config we need to make sure that org-roam follows the same directory structure as logseq. The important part of `org-roam-capture-templates` is that new files are created in `pages/` and that `org-roam-dailies-directory` is `journals/`. The rest can be customized as you like. This can be done with the config below.

```emacs-lisp
(use-package org-roam
  :custom
  (org-roam-directory "<path to logseq root>")
  (org-roam-dailies-directory "journals/")
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?" :target
      (file+head "pages/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t))))
```

One other issue is that when logseq creates a link, it will do so using a file link. But when org-roam creates a link it will do using a id link. Org-roam doesn't see file links as backlinks and logseq [doesn't see id links as backlinks](https://github.com/logseq/logseq/issues/3281#issuecomment-1059862531). It's kind of a [moose juice](https://seuss.fandom.com/wiki/Sleeping_Moose) and goose juice situation. To fix this I regularly run `org-roam-migrate-wizard`, which will convert file links to id links (among other things).


## Taking org-roam on the go {#taking-org-roam-on-the-go}

With org-roam and logseq setup, I can now access my notes anywhere. When I am around my computer I have Emacs open and take notes in org-roam. When I am out on the go, I have logseq. I finally have access to my notes everywhere and don't have to be tied to my PC. They are a perfect match.


## Screenshot {#screenshot}

{{< figure src="/images/logseq-screen-shot.png" >}}
