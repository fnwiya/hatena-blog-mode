[![MIT License](http://img.shields.io/badge/license-MIT-blue.svg?style=flat)](LICENSE)
# hatena-blog-mode
[Hatena Blog API](http://developer.hatena.ne.jp/ja/documents/blog/apis/atom) Library for Emacs

## Install
Put this package in your load-path-dir.

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

## Future Work

- [ ] get post
- [x] update post
- [x] dalete post
- [x] write in html-mode
- [ ] write in hatena-mode

## Inspired

- http://ichiroc.hatenablog.com/entry/2013/12/16/235806
- https://github.com/takaishi/ya-hatena-mode
- https://gist.github.com/tarao/4465244

## License
This software is released under the MIT License, see LICENSE.txt.
