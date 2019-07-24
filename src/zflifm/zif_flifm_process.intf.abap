interface ZIF_FLIFM_PROCESS
  public .


  types:
    tyt_dfies TYPE STANDARD TABLE OF dfies WITH DEFAULT KEY .
  types:
    tyr_hkont TYPE RANGE OF hkont .

  methods ADD_NET_PROFIT_LINES
    importing
      !IT_FIELDS type TYT_DFIES
      !IV_YTD_CHECK type ABAP_BOOL optional
    raising
      ZCX_FLIFM_EXCEPTION .
  methods CALC_RATIO_RATE
    importing
      !IS_DATA type DATA optional
    changing
      !CT_TABLE type STANDARD TABLE .
  methods ADD_NOT_ASSIGNED_LINES
    importing
      !IT_FIELDS type TYT_DFIES
      !IR_HKONT type TYR_HKONT
    raising
      ZCX_FLIFM_EXCEPTION .
endinterface.
