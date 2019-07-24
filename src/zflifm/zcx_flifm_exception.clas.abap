class ZCX_FLIFM_EXCEPTION definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_FLIFM_EXCEPTION,
      msgid type symsgid value 'ZMC_FLIFM',
      msgno type symsgno value '000',
      attr1 type scx_attrname value 'MSGV1',
      attr2 type scx_attrname value 'MSGV2',
      attr3 type scx_attrname value 'MSGV3',
      attr4 type scx_attrname value 'MSGV4',
    end of ZCX_FLIFM_EXCEPTION .
  constants C_MSG_CLASS type MSGID value 'ZMC_FLIFM' ##NO_TEXT.
  data MSGV1 type SYMSGV .
  data MSGV2 type SYMSGV .
  data MSGV3 type SYMSGV .
  data MSGV4 type SYMSGV .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type SYMSGV optional
      !MSGV2 type SYMSGV optional
      !MSGV3 type SYMSGV optional
      !MSGV4 type SYMSGV optional .
  class-methods RAISE_MSG
    importing
      !IV_MSG type STRING
      !IX_PREVIOUS type ref to CX_ROOT optional
    raising
      ZCX_FLIFM_EXCEPTION .
  class-methods RAISE_T100
    importing
      !IV_MSGID type SYMSGID optional
      !IV_MSGNO type SYMSGNO
      !IV_MSGV1 type SYMSGV optional
      !IV_MSGV2 type SYMSGV optional
      !IV_MSGV3 type SYMSGV optional
      !IV_MSGV4 type SYMSGV optional
    raising
      ZCX_FLIFM_EXCEPTION .
  class-methods RAISE_SY_MSG
    importing
      !IX_PREVIOUS type ref to CX_ROOT optional
    raising
      ZCX_FLIFM_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS ZCX_FLIFM_EXCEPTION IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
me->MSGV3 = MSGV3 .
me->MSGV4 = MSGV4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_FLIFM_EXCEPTION .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


METHOD raise_msg.

  DATA: ls_t100key TYPE scx_t100key.

  cl_message_helper=>set_msg_vars_for_clike( iv_msg ).

  ls_t100key-msgid = c_msg_class.
  ls_t100key-msgno = 000.
  ls_t100key-attr1 = 'MSGV1'.
  ls_t100key-attr2 = 'MSGV2'.
  ls_t100key-attr3 = 'MSGV3'.
  ls_t100key-attr4 = 'MSGV4'.

  RAISE EXCEPTION TYPE zcx_flifm_exception
    EXPORTING
      textid   = ls_t100key
      msgv1    = sy-msgv1
      msgv2    = sy-msgv2
      msgv3    = sy-msgv3
      msgv4    = sy-msgv4
      previous = ix_previous.

ENDMETHOD.


METHOD raise_sy_msg.

  DATA: ls_t100key TYPE scx_t100key.

  ls_t100key-msgid = sy-msgid.
  ls_t100key-msgno = sy-msgno.
  ls_t100key-attr1 = sy-msgv1.
  ls_t100key-attr2 = sy-msgv2.
  ls_t100key-attr3 = sy-msgv3.
  ls_t100key-attr4 = sy-msgv4.

  RAISE EXCEPTION TYPE zcx_flifm_exception
    EXPORTING
      textid   = ls_t100key
      previous = ix_previous.

ENDMETHOD.


METHOD raise_t100.

  DATA: ls_t100key TYPE scx_t100key.

  ls_t100key-msgid = iv_msgid.
  ls_t100key-msgno = iv_msgno.
  ls_t100key-attr1 = 'MSGV1'.
  ls_t100key-attr2 = 'MSGV2'.
  ls_t100key-attr3 = 'MSGV3'.
  ls_t100key-attr4 = 'MSGV4'.

  IF ls_t100key-msgid IS INITIAL.
    ls_t100key-msgid = c_msg_class.
  ENDIF.

  IF ls_t100key-msgno IS INITIAL.
    ls_t100key-msgno = 000.
  ENDIF.

  RAISE EXCEPTION TYPE zcx_flifm_exception
    EXPORTING
      textid = ls_t100key
      msgv1  = iv_msgv1
      msgv2  = iv_msgv2
      msgv3  = iv_msgv3
      msgv4  = iv_msgv4.

ENDMETHOD.
ENDCLASS.
