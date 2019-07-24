interface ZIF_FLIFM_ROUTER
  public .


  methods ON_ROUTING
    importing
      !IV_ROUTE type ZIF_FLIFM_DEFINITIONS=>TY_ROUTE
      !IV_MENU type ZIF_FLIFM_DEFINITIONS=>TY_FLIFM_MENU_TYPE optional
      !IV_COMPANY type ZIF_FLIFM_DEFINITIONS=>TY_FLIFM_COMPANY_TYPE optional
      !IV_ACTION type ZIF_FLIFM_DEFINITIONS=>TY_ACTION optional
      !IO_PARENT type ref to CL_GUI_CONTAINER OPTIONAL
    raising
      ZCX_FLIFM_EXCEPTION .
endinterface.
