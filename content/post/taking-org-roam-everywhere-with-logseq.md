+++
title = "Taking org-roam everywhere with logseq"
author = ["Troy Hinckley"]
date = 2021-05-26
draft = false
+++

I love [org-roam](https://www.orgroam.com/). It lets me take notes in a way that matches how I think. It makes it easy to recall what I have learned and find connections between ideas. But there has always been one big problem with org-roam, it ties me to the desktop. When I am on the go and all I have is my phone then I don't have access to them.

There are some stop gap solutions to try and make up the difference. [Beorg](https://beorgapp.com/) is iOS app that supports editing org files on mobile. However it is more focused on task management then note taking. It does not offer text search, has no way to insert "org-roam-links", and does not supported nested directories of org files. [Organice](https://organice.200ok.ch/) is another org mobile solution that can be used as PWA. However it suffers from the same limitations as beorg, and has a poor editing experience (when you switch away from the app it will close your text box, making it hard to take notes on something you are reading). In the end I was not satisfied with anything that I found. Nothing allowed the same workflow I had on desktop.

Then I happened upon logseq. It is open-source, privacy-centric note taking app inspired by org-mode and roam-research. It has native support for the org mode format. Since both org-roam and logseq are based around roam itself, I can use the same workflow on both apps. It has all the features you would expect of a roam replica, including full text search, page links, inline images/video's, etc. As a bonus it is fairly easy to setup to work with org-roam.


## configuring logseq {#configuring-logseq}

One of the requirements is that your notes must be in a github  (though they are adding support for other backends). When you go to [logseq.com](https://logseq.com/) you can setup a new repo. From there we need update some of the settings to work with org-roam. Go to `logseq/config.edn`

{{< figure src="/images/config-edn-screenshot.png" >}}

We are going to change 4 things here:

1.  Change `:preferred-format` to `Org`,
2.  Change `:preferred-workflow` to `:todo`
3.  Add `:org-mode/insert-file-link? true`
4.  Add `:date-formatter "yyyy-MM-dd"`&nbsp;[^fn:1]

In the end the start of the file will look like this

```clojure
 {
  :preferred-format Org
  :preferred-workflow :todo
  :org-mode/insert-file-link? true
  :date-formatter "yyyy-MM-dd"
...
```


## configuring org-roam {#configuring-org-roam}

For the emacs-side config we need to make sure that org-roam follows the same directory structure as logseq. The important part of `org-roam-capture-templates` is that new files are created in `pages/`.  The rest can be customized as you like. This can be done with the config below.

```emacs-lisp
(use-package org-roam
  :custom
  (org-roam-directory "<path to logseq repo root>")
  (org-roam-dailies-directory "journals/")
  (org-roam-capture-templates
   '(("d" "default" plain
      #'org-roam-capture--get-point "%?"
      :file-name "pages/${slug}" :head "#+title: ${title}\n" :unnarrowed t))))
```

One thing to keep in mind is that there is currently a [bug](https://github.com/logseq/logseq/issues/2002) in logseq where it will not follow links correctly unless the _description_ matches the _title_. For example, the link `[[file:biology.org][biological]]` will not go to the `biology.org` file, but instead try and create a new file called `biological.org`. Also logseq will not recognize this as a backlink.


## Taking org-roam on the go {#taking-org-roam-on-the-go}

With org-roam and logseq setup, I can now access my notes anywhere. When I am around my computer I have Emacs open and take notes in org-roam. When I am out on the go, I have logseq. As an added bonus, logseq supports desktop browsers (and even has a desktop app), so I use that for taking notes on my work computer (where I don't want to keep personal notes on filesystem). I finally have access to my notes everywhere and don't have to be tied to my PC. They are a perfect match.


## screenshot {#screenshot}

{{< figure src="/images/logseq-screen-shot.png" >}}

[^fn:1]: The reason this needs to be changed is because Emacs does not support ordinal dates (1st, 2nd, 3rd, 4th, etc).If you want to add support for ordinal dates, you can follow my config [here](https://github.com/CeleritasCelery/emacs.d/blob/master/emacs.org#roam).
