class /CLIN/CL_HCM_ANNO_REF_DPC_EXT definition
  public
  inheriting from /CLIN/CL_HCM_ANNO_REF_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET
    redefinition .
protected section.

  methods INFOTYPE0001SET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS /CLIN/CL_HCM_ANNO_REF_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~get_entity.

  DATA:
    lv_done TYPE abap_bool.

  /clin/hcm_anno_shlp_processor=>get_entity(
    EXPORTING
      io_tech_request_context  = io_tech_request_context
      ir_request_details       = me->mr_request_details
    IMPORTING
      ev_done                  = lv_done
      er_entity                = er_entity ).

  CHECK lv_done = abap_false.

  super->/iwbep/if_mgw_appl_srv_runtime~get_entity(
    EXPORTING
      iv_entity_name          = iv_entity_name
      iv_entity_set_name      = iv_entity_set_name
      iv_source_name          = iv_source_name
      it_key_tab              = it_key_tab
      it_navigation_path      = it_navigation_path
      io_tech_request_context = io_tech_request_context
    IMPORTING
      er_entity               = er_entity
      es_response_context     = es_response_context
         ).


ENDMETHOD.


METHOD /iwbep/if_mgw_appl_srv_runtime~get_entityset.

  DATA:
        lv_done TYPE abap_bool.

  /clin/hcm_anno_shlp_processor=>get_entityset(
    EXPORTING
      io_tech_request_context  = io_tech_request_context
      ir_request_details       = me->mr_request_details
    IMPORTING
      ev_done                  = lv_done
      er_entityset             = er_entityset
      es_response_context      = es_response_context ).

  CHECK lv_done = abap_false.



  super->/iwbep/if_mgw_appl_srv_runtime~get_entityset(
    EXPORTING
      iv_entity_name           = iv_entity_name
      iv_entity_set_name       = iv_entity_set_name
      iv_source_name           = iv_source_name
      it_filter_select_options = it_filter_select_options
      it_order                 = it_order
      is_paging                = is_paging
      it_navigation_path       = it_navigation_path
      it_key_tab               = it_key_tab
      iv_filter_string         = iv_filter_string
      iv_search_string         = iv_search_string
      io_tech_request_context  = io_tech_request_context
    IMPORTING
      er_entityset             = er_entityset
      es_response_context      = es_response_context
         ).

ENDMETHOD.


  METHOD infotype0001set_get_entityset.



    SELECT pernr subty objps sprps endda begda seqnr
           aedtm uname histo itxex refex ordex itbld preas flag1 flag2 flag3 flag4 rese1 rese2 grpvl
           bukrs werks persg persk vdsk1 gsber btrtl juper abkrs ansvh kostl orgeh plans stell mstbr sacha sachp
           sachz sname ename otype sbmod kokrs fistl geber fkber grant_nbr sgmnt budget_pd
      FROM pa0001 INTO CORRESPONDING FIELDS OF TABLE et_entityset UP TO 100 ROWS WHERE endda = '99991231'.



  ENDMETHOD.
ENDCLASS.
