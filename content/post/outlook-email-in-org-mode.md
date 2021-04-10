+++
title = "Using org mode to write email for outlook"
author = ["Troy Hinckley"]
date = 2019-02-08
categories = ["emacs"]
draft = false
+++

I see many threads on Reddit and blog posts about using email inside Emacs. I mean, I already have `org-mode` which organizing my whole digital life. But then all my work email is provided through outlook, which does not allow me to fetch email with anything other then their proprietary software.

Microsoft outlooked was designed to be used by people writing marketing emails, not people talking about code. There is no way to distinguish what is code from what is text, or call our programming symbols from the rest of the prose. Emacs `org-mode` on the other hand, was built for working in a technical environment.

Thankfully org makes it easy to export any heading to HTML. The hard part is getting that HTML into outlook. Most of the ideas presented here were taken this [post](http://bnbeckwith.com/blog/org-mode-to-outlook.html), and then expanded on.

The process I use to write my email with `org-mode` is as follows

1.  write the email in an org capture buffer
2.  Use a custom function to copy the exported HTML to the clipboard
3.  go to outlook and use a custom VBA function to insert the HTML from
    the clipboard as formatted text


## Setting up Emacs {#setting-up-emacs}


### Using an org capture buffer {#using-an-org-capture-buffer}

Using an org capture buffer is perfect for writing email, because I can save it as a draft if needed, or export the contents and then throw the buffer away. Also, most of the time, the content of interest is what I am working on that moment, so I have everything at hand. Here is the simple template that I use to write emails.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(add-to-list 'org-capture-templates
             '("e" "Email" entry (expand-file-name "email.org" org-directory)
               "* %?" :empty-lines 1))
{{< /highlight >}}


### Exporting from org-mode {#exporting-from-org-mode}

Normally if you wanted to export an org header as HTML, you would use `C-c C-e` to open the export menu. `hH` will open a dedicated buffer with the HTML contents of your org file. From there you can copy the whole buffer. However I find it much faster to use this helper function (bound to `C-x M-e`).

{{< highlight emacs-lisp "linenos=table, linenostart=4" >}}
(defun export-org-email ()
  "Export the current email org buffer and copy it to the clipboard"
  (interactive)
  (let ((org-export-show-temporary-export-buffer nil)
        (org-html-head (org-email-html-head)))
    (org-html-export-as-html)
    (with-current-buffer "*Org HTML Export*"
      (kill-new (buffer-string)))
    (message "HTML copied to clipboard")))
{{< /highlight >}}


### Better CSS for export {#better-css-for-export}

The default HTML exported by org is spartan to say the least. Thankfully it is pretty easy to define some custom to CSS that looks prettier and plays nicer with outlooks HTML rendering engine. The outlook compatible HTML I use is located [here](https://github.com/CeleritasCelery/org-html-themes/blob/master/styles/email/css/email.css). The function below adds my CSS to `org-html-head`. It is called by `export-org-email` from the previous section.

As you can see in the function below, I have this CSS at the local path `~/org/org-html-themes/styles/email/css/email.css`. You will need to change this to where ever you keep the CSS file.

{{< highlight emacs-lisp "linenos=table, linenostart=13" >}}
(defun org-email-html-head ()
  "Create the header with CSS for use with email"
  (concat
   "<style type=\"text/css\">\n"
   "<!--/*--><![CDATA[/*><!--*/\n"
   (with-temp-buffer
     (insert-file-contents
      "~/org/org-html-themes/styles/email/css/email.css")
     (buffer-string))
   "/*]]>*/-->\n"
   "</style>\n"))
{{< /highlight >}}


## setting up outlook {#setting-up-outlook}


### Getting the HTML into outlook {#getting-the-html-into-outlook}

This is the tricky part. outlook does not make it easy to insert HTML inline. I had to learn some VBA and use the outlook code editor. I hope I never have to do that again.

To add a function to outlook

1.  Press `Alt-F11` to bring up the VBA editor.
2.  You should see the default project. Change this project name to something more appropriate. Note that the Project name **MUST NOT** be the name of the function (`PrependClipboardHTML`) so name it something else.
3.  Right click on the project to add a new module and copy in the function from below

<!--listend-->

{{< highlight cs "linenos=table, linenostart=1" >}}
Sub PrependClipboardHTML()
    Dim email As Outlook.MailItem
    Dim cBoard As DataObject

    Set email = Application.ActiveInspector.CurrentItem
    Set cBoard = New DataObject

    cBoard.GetFromClipboard
    email.HTMLBody = cBoard.GetText + email.HTMLBody

    Set cBoard = Nothing
    Set email = Nothing
End Sub
{{< /highlight >}}


### Fix object library {#fix-object-library}

This step may not apply to everyone, but in order to get this to work, I also had to add the `Microsoft Forms 2.0 Object Library` to the References. I figured this out by looking at [this](https://www.reddit.com/r/orgmode/comments/74k2mx/org%5Flink%5Fto%5Fmessage%5Fwithin%5Foutlook%5F2016/) Reddit thread.

1.  Click on `Tools` in the menu bar (or use `Alt-t`).
2.  Select `References...`
3.  Select `Browse...`
4.  Browse to `C:\Windows\System32\FM20.DLL` and select open


### Add to Quick Access Toolbar {#add-to-quick-access-toolbar}

This function only makes sense in context of an email. To enable it there, add it to the quick access toolbar at the top.

1.  Press `Ctrl-n` to open up a new email.
2.  Select the little down arrow at the very top for the `Customize Quick Access Toolbar` menu.
3.  Select `More Commands`.
4.  In the drop down for `choose commands from:` select `Macros`. You should see the `PrependClipboardHTML` macro you created here.
5.  Add it to the right hand side pane with the `Add >>` button.
6.  Click on `Modify...` to change the icon and display name. You can also use the arrow to change the ordering in the `Quick Access Toolbar`

Now clicking on that button will copy clipboard contents into the email as HTML. Our raw HTML exported from Org mode gets inserted nicely and we gain the formatting desired.

The other bonus (or maybe the main point) is that now you can also use a built-in shorcut for the Quick Access Toolbar commands to run this one. By pressing `Alt`, you can see a number by your command.


### Matching the default font in Outlook {#matching-the-default-font-in-outlook}

It is nice sometimes to have the default font in outlook match what you are exporting from org mode. To make this happen, do the following steps in Outlook.

1.  On the `File` tab, choose `Options` > `Mail`.
2.  Under `Compose messages`, choose `Stationery and Fonts`.
3.  On the `Personal Stationery` tab, under `New mail messages` or `Replying or forwarding messages`, choose Font.
4.  In the `Font` box, choose the font, style, size, and color that you want to use. You can see a preview of your changes as you make them.
5.  Choose `OK` three times to return to Outlook.


## Bonus Content {#bonus-content}

Here are a few more ideas that are not necessary for this workflow but
are useful to me.


### More advanced VBA {#more-advanced-vba}

The `PrependClipboardHTML` function I showed above is not actually the version I use. But I chose to mention present it as the solution because it is simple and works well. This more advanced version has two differences

1.  Works with inline email replies
2.  If the subject line is empty, the HTML header at the start of the body is used as the subject line. This allows you add the subject line in org-mode and have it automatcially inserted.

<!--listend-->

{{< highlight cs "linenos=table, linenostart=1" >}}
Sub PrependClipboardHTML()
    Dim email As Outlook.MailItem
    Dim cBoard As DataObject

    Set email = GetCurrentItem()
    Set cBoard = New DataObject

    cBoard.GetFromClipboard

    Dim sText As String
    Dim headerStart As Integer
    Dim headerStartClose As Integer
    Dim HTMLPre As String
    Dim HTMLPost As String
    Dim subject As String
    Dim paragraphStart As Integer

    Dim headerEndStr As String
    Const titleText = "<h1 class=""title"">"

    headerEndStr = "</h1>"
    headerStart = Len(titleText)

    sText = cBoard.GetText
    HTMLPre = sText

    headerStart = InStr(sText, titleText)
    If Not headerStart > 0 Then
        ' if no title text, we use the starting header
        headerStart = InStr(sText, "<h2 id=")
        headerEndStr = "</h2>"
    End If

    ' we use the first header as the subject line if the subject line is empty
    If headerStart > 0 Then
        headerStartClose = InStr(headerStart, sText, ">") + 1
        Dim headerEnd As Integer
        headerEnd = InStr(headerStartClose, sText, headerEndStr)
        If headerEnd > 0 Then
            subject = Mid(sText, _
                headerStartClose, _
                headerEnd - headerStartClose)
            HTMLPre = Mid(sText, 1, headerStart - 1) ' arrays start at 1...
            HTMLPost = Mid(sText, headerEnd + Len(headerEndStr))
        End If
    End If

    email.HTMLBody = HTMLPre + HTMLPost + email.HTMLBody
    ' only use the HTML subject if an email subject is absent
    If Len(email.subject) = 0 Then
        email.subject = subject
    End If

    ' deallocate objects
    Set cBoard = Nothing
    Set email = Nothing

End Sub

' Get either the active email item or the current window
Function GetCurrentItem() As Object
    Dim objApp As Outlook.Application

    Set objApp = Application

    On Error Resume Next
    Select Case TypeName(objApp.ActiveWindow)
        Case "Explorer"
            Set GetCurrentItem = objApp.ActiveExplorer.ActiveInlineResponse
        Case "Inspector"
            Set GetCurrentItem = objApp.activeInspector.CurrentItem
    End Select

    Set objApp = Nothing
End Function
{{< /highlight >}}


### Normalize outlook formatting {#normalize-outlook-formatting}

Unless you disable it, outlook will try and "prettify" some characters as you type with non ascii-compatible versions. This means that you will often encounter errors when copying code out of outlook and trying to paste into a shell or source file. The following function takes the last paste normalizes it to be ascii compatible.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun normalize-text (beg end)
  "normalize characters used in Microsoft formatting"
  (let* ((orig-text (buffer-substring beg end))
         (normalized-text
          (thread-last orig-text
            (replace-regexp-in-string "–" "--")
            (replace-regexp-in-string (rx (char "‘’")) "'")
            (replace-regexp-in-string (rx (char "“”")) "\""))))
    (unless (equal orig-text normalized-text)
      (save-excursion
        (goto-char beg)
        (delete-region beg end)
        (insert normalized-text)))))

(defun normalize-region (beg end)
  "normalize the last paste, or if region is selected, normalize
that region."
  (interactive "r")
  (if (region-active-p)
      (progn (normalize-text beg end)
             (deactivate-mark))
    (apply #'normalize-text (cl-sort (list (point) (mark t)) '<))))
{{< /highlight >}}


### Have a comment? {#have-a-comment}

View the discussion on [Reddit](https://www.reddit.com/r/emacs/comments/gxrg0b/writing%5Femail%5Ffor%5Foutlook%5Finside%5Femacs/?utm%5Fsource=share&utm%5Fmedium=web2x&context=3) or send me an email
