" See colors at https://jonasjacek.github.io/colors/



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" PLUGINS (vim-plug)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
call plug#begin()
let g:plug_window = 'bot new | bot res 10'
Plug 'preservim/nerdtree'
Plug 'tpope/vim-sleuth'

" Intellisense
Plug 'neoclide/coc.nvim'
let g:coc_global_extensions = ['coc-tsserver', 'coc-json']

Plug 'vim-airline/vim-airline'
let g:airline#extensions#tabline#enabled = 1
let g:airline_symbols_ascii = 1
let g:airline_focuslost_inactive = 1

call plug#end()



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" APPEARANCE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" INDENTATION, TABS, RULERS, NUMBERS
set smartindent
set expandtab
set ruler
set number relativenumber


" HIGHLIGHTING
syntax on

" Ruler
set colorcolumn=80
highlight ColorColumn ctermbg=0 guibg=lightgrey

" CursorLine
set cursorline
highlight CursorLine ctermbg=235 cterm=none



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" REMAPPINGS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <C-n> :NERDTreeRefreshRoot<CR>:NERDTreeToggle<CR>
map <C-b> :buffers<CR>:buffer<Space>

" Clears search highlight
map <C-s> :let @/ = ""<CR>

" Buffer Nav
map <Leader>] :bn<CR>
map <Leader>[ :bp<CR>

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



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" FILE TYPE ASSOCIATIONS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
au BufNewFile,BufRead Jenkinsfile setf groovy
au BufNewFile,BufRead *.tsx set syntax=typescript

