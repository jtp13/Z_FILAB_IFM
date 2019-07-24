*&---------------------------------------------------------------------*
*& MIT License
*&
*& Copyright (c) 2016 FI LAB. All rights reserved.
*&
*& Permission is hereby granted, free of charge, to any person obtaining
*& a copy of this software and associated documentation files (the "Software"),
*& to deal in the Software without restriction, including without limitation
*& the rights to use, copy, modify, merge, publish, distribute, sublicense,
*& and/or sell copies of the Software, and to permit persons to whom the
*& Software is furnished to do so, subject to the following conditions:
*&
*& The above copyright notice and this permission notice shall be included
*& in all copies or substantial portions of the Software.
*&
*& THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
*& OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*& FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*& AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*& LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*& OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
*& IN THE SOFTWARE.
*&---------------------------------------------------------------------*
class ZCL_FLIFM_SERVICES_FILAB definition
  public
  create public .

public section.

  constants MC_FILAB_HOMEPAGE type STRING value 'http://www.fi-lab.com' ##NO_TEXT.
  constants MC_FILAB_HELP type STRING value 'https://tawk.to/chat/59ed41a7c28eca75e46277ea/default' ##NO_TEXT.

  class-methods OPEN_FILAB_HOMEPAGE
    raising
      ZCX_FLIFM_EXCEPTION .
  class-methods OPEN_FILAB_HELP
    raising
      ZCX_FLIFM_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FLIFM_SERVICES_FILAB IMPLEMENTATION.


  method OPEN_FILAB_HELP.


    cl_gui_frontend_services=>execute(
      EXPORTING document = mc_filab_help
      EXCEPTIONS OTHERS = 1 ).

    IF sy-subrc <> 0.
      zcx_flifm_exception=>raise_msg( 'Opening "FI LAB help page" failed.' ).
    ENDIF.


  endmethod.


  method OPEN_FILAB_HOMEPAGE.


    cl_gui_frontend_services=>execute(
      EXPORTING document = mc_filab_homepage
      EXCEPTIONS OTHERS = 1 ).

    IF sy-subrc <> 0.
      zcx_flifm_exception=>raise_msg( 'Opening "http://www.fi-lab.com" failed.' ).
    ENDIF.


  endmethod.
ENDCLASS.
