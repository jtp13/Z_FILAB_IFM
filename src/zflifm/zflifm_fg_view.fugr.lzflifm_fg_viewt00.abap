*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2019.05.20 at 20:13:04
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZFLIFMV_LAYOUT..................................*
TABLES: ZFLIFMV_LAYOUT, *ZFLIFMV_LAYOUT. "view work areas
CONTROLS: TCTRL_ZFLIFMV_LAYOUT
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZFLIFMV_LAYOUT. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZFLIFMV_LAYOUT.
* Table for entries selected to show on screen
DATA: BEGIN OF ZFLIFMV_LAYOUT_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZFLIFMV_LAYOUT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFLIFMV_LAYOUT_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZFLIFMV_LAYOUT_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZFLIFMV_LAYOUT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFLIFMV_LAYOUT_TOTAL.

*...processing: ZFLIFMV_TCURR...................................*
TABLES: ZFLIFMV_TCURR, *ZFLIFMV_TCURR. "view work areas
CONTROLS: TCTRL_ZFLIFMV_TCURR
TYPE TABLEVIEW USING SCREEN '0002'.
DATA: BEGIN OF STATUS_ZFLIFMV_TCURR. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZFLIFMV_TCURR.
* Table for entries selected to show on screen
DATA: BEGIN OF ZFLIFMV_TCURR_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZFLIFMV_TCURR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFLIFMV_TCURR_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZFLIFMV_TCURR_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZFLIFMV_TCURR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFLIFMV_TCURR_TOTAL.

*.........table declarations:.................................*
TABLES: ZFLIFMT_LAYOUT                 .
TABLES: ZFLIFMT_TCURR                  .
