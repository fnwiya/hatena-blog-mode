# hatena-blog-mode
Hatena Blog API Library for Emacs

## Usage
### Setting
```lisp
(require 'hatena-blog-mode.el)
(setq hatena-id "XXXXXXXX")
(setq hatena-blog-api-key  "XXXXXXXX")
(setq hatena-blog-id "XXXXXXXX")
(setq hatena-blog-editing-mode "md")     ;; set md or html(default as md)
(setq hatena-blog-backup-dir "XXXXXXXX") ;; set if you want to backup your post.
```

### Post

0. ```M-x hatena-blog-write```
0. write your entry
0. ```M-x hatena-blog-post```

## Install
put this package in your load-path-dir

## Future Work

* get/update/dalete your post
* write in ~~htmk-mode &~~ hatena-mode

## License
MIT License
