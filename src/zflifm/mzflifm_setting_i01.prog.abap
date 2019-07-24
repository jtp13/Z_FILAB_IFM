*&---------------------------------------------------------------------*
*&  Include           MZFLIFM_SETTING_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  g_okcode = gv_okcode.
  CLEAR gv_okcode.

  CASE g_okcode.
    WHEN 'SAVE'.

      PERFORM save_data.
      PERFORM set_initial_data.

    WHEN 'FC01'.
      CALL TRANSACTION 'ZFLIFMV_LAYOUT'.

    WHEN 'FC02'.
      CALL TRANSACTION 'ZFLIFMV_TCURR'.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  LEAVE PROGRAM.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_CURSOR_FIELD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_cursor_field INPUT.

  CLEAR: gv_cursor_field, gv_cursor_line.
  GET CURSOR FIELD gv_cursor_field  LINE gv_cursor_line .

ENDMODULE.                 " GET_CURSOR_FIELD  INPUT
