interface ZIF_FLIFM_POPUPS
  public .


  methods SHOW_FSV_LIST_POPUP
    importing
      !IV_MENU type ZIF_FLIFM_DEFINITIONS=>TY_FLIFM_MENU_TYPE
      !IT_NKEY type LVC_T_NKEY
      !IT_POPUP_TABLE type STANDARD TABLE
    raising
      ZCX_FLIFM_EXCEPTION .
  methods SHOW_NET_PROFIT_LIST_POPUP
    importing
      !IV_MENU type ZIF_FLIFM_DEFINITIONS=>TY_FLIFM_MENU_TYPE
      !IV_NAME type SEU_NAME
      !IV_FIELDNAME type LVC_FNAME
      !IV_KOMOK type KOMOK
      !IT_POPUP_TABLE type STANDARD TABLE .
endinterface.
