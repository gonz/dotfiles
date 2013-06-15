" PLUGINS
" =======
"


"" Plugin: Pathogen

call pathogen#infect()


"" Plugin: NERDTree

" set project folder to x
map ´f :NERDTreeToggle<CR>
nnoremap ´c :NERDTreeFind<CR>
map ´b :NERDTreeFromBookmark<Space>
" files/dirs to ignore in NERDTree
let NERDTreeIgnore=[
    \'\~$',
    \'\.pt.cache$',
    \'\.Python$',
    \'\.svn$',
    \'\.git*$',
    \'\.pyc$',
    \'\.pyo$',
    \'\.mo$',
    \'\.o$',
    \'\.lo$',
    \'\.la$',
    \'\..*.rej$',
    \'\.rej$',
    \'\.\~lock.*#$',
    \'\.AppleDouble$',
    \'\.DS_Store$']
let NERDTreeChDirMode=2

