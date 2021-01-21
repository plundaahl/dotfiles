" See colors at https://jonasjacek.github.io/colors/


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" PLUGINS (vim-plug)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
call plug#begin()
let g:plug_window = 'bot new | bot res 10'
Plug 'preservim/nerdtree'
Plug 'tpope/vim-sleuth'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }

" Intellisense
Plug 'neoclide/coc.nvim'
let g:coc_global_extensions = ['coc-tsserver', 'coc-json', 'coc-python', 'coc-eslint']
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
let g:go_def_mode='gopls'
let g:go_info_mode='gopls'

Plug 'vim-airline/vim-airline'
let g:airline#extensions#tabline#enabled = 1
let g:airline_symbols_ascii = 1
let g:airline_focuslost_inactive = 1

Plug 'jremmen/vim-ripgrep'
let g:rg_highlight = 1
let g:rg_derive_root = 1

" Typescript Highlighting (see https://github.com/peitalin/vim-jsx-typescript)
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'

call plug#end()



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" APPEARANCE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" INDENTATION, TABS, RULERS, NUMBERS
set smartindent
set expandtab
set ruler
set number relativenumber

map <Leader><Tab>1 :call Indent(1)<CR>
map <Leader><Tab>2 :call Indent(2)<CR>
map <Leader><Tab>3 :call Indent(4)<CR>
map <Leader><Tab>4 :call Indent(8)<CR>

function! Indent(indent) abort
  :exec 'set shiftwidth=' . a:indent
  :exec 'set tabstop=' . a:indent
endfunction


" HIGHLIGHTING
syntax on

" Ruler
set colorcolumn=80
highlight ColorColumn ctermbg=0 guibg=lightgrey

" CursorLine
set cursorline
set cursorcolumn
highlight CursorLine ctermbg=235 cterm=none
highlight CursorColumn ctermbg=235 cterm=none



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" REMAPPINGS & BEHAVIOR
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set mouse=a
set autoread

map <C-n> :call ToggleNerdtree()<CR>
map <Leader>b :buffers<CR>:buffer<Space>
map <Leader>o :bp<CR>
map <Leader>p :bn<CR>
map <Leader>q :close<CR>
map <Leader>x :bd<CR>

" Clears search highlight
map <Leader>c :let @/ = ""<CR>:cclose<CR>

" FZF
map <Leader>f :FZF<CR>

" Buffer Nav
map <Leader>] :bn<CR>
map <Leader>[ :bp<CR>

" Refresh All Files
map <Leader>r :bufdo e<CR>

" On Save...
" - Delete trailing whitespace
autocmd BufWritePre * %s/\s\+$//e

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" PLUGIN CONFIG
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" LANGUAGE SERVER AUTOCOMPLETE

" use <tab> for trigger completion and navigate to the next complete item
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<Tab>" :
      \ coc#refresh()


" NERDTree
let NERDTreeQuitOnOpen = 1

" Close NERDTree if it's the only remaining buffer
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

function! ToggleNerdtree() abort
  if (exists("b:NERDTree") && b:NERDTree.isTabTree())
    :NERDTreeToggle
  elseif (expand('%:p') != '')
    :NERDTreeFind
  else
    :exec 'NERDTreeToggle '.getcwd()
  endif
  :NERDTreeRefreshRoot
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" FILE TYPE ASSOCIATIONS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
au BufNewFile,BufRead Jenkinsfile setf groovy
au BufNewFile,BufRead *.tsx,*.jsx set syntax=typescript

