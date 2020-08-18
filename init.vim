" Plugins (vim-plug)
call plug#begin()
let g:plug_window = 'bot new | bot res 10'
Plug 'neoclide/coc.nvim', {'branch': 'v0.0.77'}
Plug 'preservim/nerdtree'
call plug#end()

" Tabs
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab

" Line Numbers
set number relativenumber

" Ruler
set colorcolumn=80
highlight ColorColumn ctermbg=0 guibg=lightgrey

" CursorLine
set cursorline
highlight CursorLine ctermbg=235 cterm=none

" use <tab> for trigger completion and navigate to the next complete item
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<Tab>" :
      \ coc#refresh()
