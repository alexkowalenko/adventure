! Common block

COMMON RTEXT, text_lines, text_lines_index
integer, parameter :: MAX_TEXT = 1000
character(len=80) text_lines(MAX_TEXT)
integer text_lines_index(MAX_TEXT)
integer :: RTEXT(100)
