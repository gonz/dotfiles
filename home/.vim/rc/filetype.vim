" Fish shell scripts
autocmd BufNewFile,BufRead *.fish setfiletype fish

" Django HTML templates
" autocmd BufRead,BufNewFile *.html setfiletype htmldjango

" Ruby config files
au! BufNewFile,BufRead config.ru,Capfile,Thorfile,*.thor,Rakefile,Vagrantfile,*.prawn,Gemfile setfiletype ruby

" Omnicompletion
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType html setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
