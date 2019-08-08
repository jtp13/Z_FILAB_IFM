interface ZIF_FLIFM_GUI
  public .


  data MS_ROUTE_DATA type ZIF_FLIFM_DEFINITIONS=>TYS_ROUTE_DATA .
  data MO_FETCH type ref to ZCL_FLIFM_FETCH .
  data MO_PROCESS type ref to ZCL_FLIFM_PROCESS .
  data MO_SPLITTER_RIGHT type ref to CL_GUI_SPLITTER_CONTAINER .
  class-data MO_ALV_TREE type ref to CL_GUI_ALV_TREE .
  class-data MO_ALV_GRID type ref to CL_GUI_ALV_GRID .

  methods RENDER
    importing
      !IV_MENU type ZIF_FLIFM_DEFINITIONS=>TY_FLIFM_MENU_TYPE
      !IV_COMPANY type ZIF_FLIFM_DEFINITIONS=>TY_FLIFM_COMPANY_TYPE
      !IV_ACTION type ZIF_FLIFM_DEFINITIONS=>TY_ACTION
    raising
      ZCX_FLIFM_EXCEPTION .
  methods CLEAN_UP .
endinterface.
