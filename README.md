# hatena-blog-mode
Hatena Blog API Library for Emacs

## Requirement


## Usage
### Setting
```lisp
(require 'hatena-blog-mode.el)
(setq hatena-id "XXXXXXXX")
(setq hatena-blog-api-key  "XXXXXXXX")
(setq hatena-blog-id "XXXXXXXX")
(setq hatena-blog-file-path "XXXXXXXX")
(setq hatena-blog-backup-dir "XXXXXXXX")
(setq hatena-blog-xml-template "<?xml version='1.0' encoding='utf-8'?>
<entry xmlns='http://www.w3.org/2005/Atom'
       xmlns:app='http://www.w3.org/2007/app'>
  <title>%s</title>
  <author><name>%s</name></author>
  <content type='text/plain'>%s</content>
  <updated>%s</updated>
  <category term='%s' />
  <app:control>
    <app:draft>%s</app:draft>
  </app:control>
</entry>")
```

### Post


## ToDo


## License
MIT License
