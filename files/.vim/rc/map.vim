" MAPPINGS
" ========
" TODO: Move this this into a helpfile "custom maps"
"
"  ,' - set invhlsearch
"  ,n - set invnumber
"
"  ,e - :e in current file dir
"  ,s - :sp in current file dir
"  ,v - :vsp in current file dir
"  ,t - :tabnew in current file dir
"
" SPELL
"
"  ,les - set spelllang=es
"  ,len - set spelllang=en
"  ,ll - set invspell
"
" WINDOWS
"
"  C-h - <C-W>h
"  C-h - <C-W>j
"  C-h - <C-W>k
"  C-h - <C-W>l
"
"  + - <C-W>+
"  - - $
"  ` - <C-W>_
"
" TABS
"
"  D-{0-9} - {0-9}gt
"  sn - tabnew
"  sc - tabclose
"  sh - tabfirst
"  sj - tabnext
"  sk - tabprevious
"  sl - tablast
"  st - tabs
"
" FOLDS
"
"  <SPACE> - Toggle fold
"  ,zi - Fold method indent
"  ,zs - Fold method syntax
"  ,zm - Fold method manual
"  ,zk - Fold method marker
"
" PLUGIN.NERDTree
"
"  ´f - Nerdtree toggle
"  ´n - Nerdtree toggle current dir
"  ,c - Nerdtree bookmark
"
" PLUGIN.NERDCommenter (leader: ",c")
"
"  ,c<space> - Toggle
"  ,cu - Uncomment
"  ,cc - Comment
"  ,cl - AlignLeft
"  ,cm - Minimal
"  ,cs - Sexy
"  ,ci - Invert
"  ,cy - Yank
"  ,cb - AlignBoth
"  ,cn - Nest
"  ,c$ - ToEOL
"  ,cA - Append
"  ,ca  - AltDelims
"

let mapleader = ","

"" Remaps
map j gj
map k gk
nmap Y y$
noremap ' `
" Toggle line numbers
map <Leader>n :set invnumber<CR>
" Toggle highlight
nmap <silent> <Leader>, :set invhlsearch<CR>


"" Folds
fun! ToggleFold()
    if foldlevel('.') == 0
        normal! l
    else
        if foldclosed('.') < 0
            . foldclose
        else
            . foldopen
        endif
    endif
    " Clear status line
    echo
endfun

noremap <space> :call ToggleFold()<CR>
"Fold method: Indent
map <silent> <leader>zi :silent set foldmethod=indent<CR>
"Fold method: manual
map <silent> <leader>zm :silent set foldmethod=manual<CR>
"Fold method: syntax
map <silent> <leader>zs :silent set foldmethod=manual<CR>
"Fold method: marker
map <silent> <leader>zk :silent set foldmethod=marker<CR>


"" Take current file basedir
" :e
map <leader>e :e <C-R>=expand("%:p:h") . "/" <CR>
" :sp
map <leader>s :sp <C-R>=expand("%:p:h") . "/" <CR>
" :tabnew
map <leader>t :tabnew <C-R>=expand("%:p:h") . "/" <CR>


"indent

"nmap <D-[> <<
"nmap <D-]> >>
"vmap <D-[> <gv
"vmap <D-]> >gv


"" Spell check
nmap <Leader>les :set spelllang=es<CR>
nmap <Leader>len :set spelllang=en<CR>
nmap <Leader>ll :setlocal invspell<CR>

"" Windows
noremap <C-h> <C-W>h
noremap <C-j> <C-W>j
noremap <C-k> <C-W>k
noremap <C-l> <C-W>l

map + <C-W>+
map - $
map ` <C-W>_

"" Tabs
map <D-1> 1gt
map <D-2> 2gt
map <D-3> 3gt
map <D-4> 4gt
map <D-5> 5gt
map <D-6> 6gt
map <D-7> 7gt
map <D-8> 8gt
map <D-9> 9gt
map <D-0>10 10gt
map <D-0>11 10gt

nmap sn :tabnew<CR>
nmap sc :tabclose<CR>

nmap sh :tabfirst<CR>
nmap sj :tabnext<CR>
nmap sk :tabprevious<CR>
nmap sl :tablast<CR>
nmap st :tabs<CR>

"" Fullscreen
nmap <D-CR> :set invfu<CR>
