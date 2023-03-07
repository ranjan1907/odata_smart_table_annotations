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

  methods EMPLOYEEDATASET_CREATE_ENTITY
    redefinition .
  methods EMPLOYEEDATASET_DELETE_ENTITY
    redefinition .
  methods EMPLOYEEDATASET_GET_ENTITY
    redefinition .
  methods EMPLOYEEDATASET_GET_ENTITYSET
    redefinition .
  methods EMPLOYEEDATASET_UPDATE_ENTITY
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


METHOD employeedataset_create_entity.

* Work Area declaration
  DATA: ls_input_data TYPE /clin/cl_hcm_anno_ref_mpc=>ts_employeedata.

* Internal Table/Range Table declaration
  DATA: lt_return TYPE bapiret2_t,                  " return message
        lt_error  TYPE bapiret2_t.                  " return message

* Class Object Declaration
  DATA: lo_meco    TYPE REF TO /iwbep/if_message_container,  " message container
        lo_objname TYPE REF TO /clin/cl_hcm_anno_ref.

* Constant  declaration
  CONSTANTS:lc_msgtyp   TYPE cc_string  VALUE 'EA'.

* Field Symbol  declaration
  FIELD-SYMBOLS: <lfs_return> TYPE bapiret2.                    " return message

  CLEAR : er_entity.

* Read Request Data
  IF io_data_provider IS BOUND.
    io_data_provider->read_entry_data( IMPORTING es_data = ls_input_data ).
    er_entity = ls_input_data.
  ENDIF.

**********************************************************************
*Add Custom class logic here

  IF lo_objname IS NOT BOUND.
    CREATE OBJECT lo_objname.
  ENDIF.

  IF lo_objname IS BOUND.
    CALL METHOD lo_objname->create_entity
      EXPORTING
        is_input  = ls_input_data         "Input param Description
      IMPORTING
        er_entity = er_entity             "Current Entiity Structure
        et_return = lt_return.
  ENDIF.
**********************************************************************
  " check return code
  LOOP AT lt_return ASSIGNING <lfs_return>               "#EC CI_STDSEQ
  " less than 500 lines
  WHERE type CA lc_msgtyp.
    INSERT <lfs_return> INTO TABLE lt_error.
  ENDLOOP.

  IF lt_error IS NOT INITIAL.

    " check that mo_context is already bound before it is being used later
    IF mo_context IS NOT BOUND.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
        EXPORTING
          textid = /iwbep/cx_mgw_tech_exception=>internal_error.
    ENDIF.

    lo_meco = mo_context->get_message_container( ).

    IF lo_meco IS BOUND.
      lo_meco->add_messages_from_bapi( it_bapi_messages = lt_error ).
    ENDIF.

    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_meco
        textid            = /iwbep/cx_mgw_busi_exception=>business_error_unlimited
        message_unlimited = iv_entity_name.

  ENDIF.


ENDMETHOD.


METHOD employeedataset_delete_entity.

* Work Area declaration
  DATA: ls_input_data TYPE /clin/cl_hcm_anno_ref_mpc=>ts_employeedata,
        ls_textid     TYPE scx_t100key.  " Error Message Text...

* Internal Table/Range Table declaration
  DATA: lt_return TYPE bapiret2_t,                  " return message
        lt_error  TYPE bapiret2_t,                  " return message
        lt_keys   TYPE /iwbep/t_mgw_tech_pairs.

* Class Object Declaration
  DATA: lo_meco    TYPE REF TO /iwbep/if_message_container,  " message container
        lo_objname TYPE REF TO /clin/cl_hcm_anno_ref.

* Local variable declaration
  DATA: lv_pernr          TYPE  /clin/cl_hcm_anno_ref_mpc=>ts_employeedata-pernr.

* Field symbol declaration
  FIELD-SYMBOLS: <lfs_keys>   TYPE /iwbep/s_mgw_tech_pair,
                 <lfs_return> TYPE bapiret2.                    " return message

* Constatnst declaration
  CONSTANTS:lc_msgtyp TYPE cc_string  VALUE 'EA',
            lc_pernr  TYPE cc_string  VALUE 'PERNR'.

  lt_keys = io_tech_request_context->get_keys( ).
  "Get Key values
  READ TABLE lt_keys ASSIGNING <lfs_keys> WITH KEY name = lc_pernr.
  IF sy-subrc = 0.
    lv_pernr = <lfs_keys>-value.
  ENDIF.

**********************************************************************
*Add Custom class logic here

  IF lo_objname IS NOT BOUND.
    CREATE OBJECT lo_objname.
  ENDIF.
  IF lo_objname IS BOUND.
    CALL METHOD lo_objname->delete_entity
      EXPORTING
        iv_pernr  = lv_pernr
      IMPORTING
        et_return = lt_return.
  ENDIF.
**********************************************************************
  " check return code
  LOOP AT lt_return ASSIGNING <lfs_return>               "#EC CI_STDSEQ
  " less than 500 lines
  WHERE type CA lc_msgtyp.
    INSERT <lfs_return> INTO TABLE lt_error.
  ENDLOOP.

  IF lt_error IS NOT INITIAL.

    " check that mo_context is already bound before it is being used later
    IF mo_context IS NOT BOUND.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
        EXPORTING
          textid = /iwbep/cx_mgw_tech_exception=>internal_error.
    ENDIF.

    lo_meco = mo_context->get_message_container( ).

    IF lo_meco IS BOUND.
      lo_meco->add_messages_from_bapi( it_bapi_messages = lt_error ).
    ENDIF.

    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_meco
        textid            = /iwbep/cx_mgw_busi_exception=>business_error_unlimited
        message_unlimited = iv_entity_name.

  ENDIF.

ENDMETHOD.


METHOD employeedataset_get_entity.

* Class object declaration
  DATA: lo_meco    TYPE REF TO /iwbep/if_message_container,  " message container
        lo_objname TYPE REF TO /clin/cl_hcm_anno_ref.

* Internal Table/Range table declaration
  DATA: lt_keys   TYPE  /iwbep/t_mgw_tech_pairs,
        lt_return TYPE bapiret2_t,                  " return message
        lt_error  TYPE bapiret2_t.

* Local variable declaration
  DATA: lv_pernr TYPE  /clin/cl_hcm_anno_ref_mpc=>ts_employeedata-pernr.

* Field symbol declaration
  FIELD-SYMBOLS: <lfs_keys>   TYPE /iwbep/s_mgw_tech_pair,
                 <lfs_return> TYPE bapiret2.                    " return message

* Constant  declaration
  CONSTANTS:lc_msgty  TYPE symsgty    VALUE 'E',
            lc_msgid  TYPE symsgid    VALUE '/IWBEP/MC_SB_DPC_ADM',
            lc_pernr  TYPE cc_string  VALUE 'PERNR',
            lc_msgtyp TYPE cc_string  VALUE 'EA'.

  CLEAR er_entity.
*-------------------------------------------------------------------------*
*  Get KEY values
*-------------------------------------------------------------------------*
* Get Filter object
  IF io_tech_request_context IS BOUND.
    lt_keys = io_tech_request_context->get_keys( ).

    "Create Range Table for key value
    READ TABLE lt_keys ASSIGNING <lfs_keys> WITH KEY name = lc_pernr .
    IF sy-subrc = 0.
      lv_pernr = <lfs_keys>-value.
    ENDIF.
  ENDIF.
*-------------------------------------------------------------------------*
*  Call Business class and error handling. Resultset of business
*  class should be assgined to ER_ENTITY
*-------------------------------------------------------------------------*
* Insert your Code here

  IF lo_objname IS NOT BOUND.
    CREATE OBJECT lo_objname.
  ENDIF.

  IF lo_objname IS BOUND.
    CALL METHOD lo_objname->get_entity
      EXPORTING
        iv_pernr  = lv_pernr
      IMPORTING
        er_output = er_entity            "Current Entiity
        et_return = lt_return.
  ENDIF.
*-------------------------------------------------------------------------*
* check return code
  LOOP AT lt_return ASSIGNING <lfs_return>               "#EC CI_STDSEQ
  " less than 500 lines
  WHERE type CA lc_msgtyp.
    INSERT <lfs_return> INTO TABLE lt_error.
  ENDLOOP.

  IF lt_error IS NOT INITIAL.

    " check that mo_context is already bound before it is being used later
    IF mo_context IS NOT BOUND.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
        EXPORTING
          textid = /iwbep/cx_mgw_tech_exception=>internal_error.
    ENDIF.

    lo_meco = mo_context->get_message_container( ).

    IF lo_meco IS BOUND.
      lo_meco->add_messages_from_bapi( it_bapi_messages = lt_error ).
    ENDIF.

    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_meco
        textid            = /iwbep/cx_mgw_busi_exception=>business_error_unlimited
        message_unlimited = iv_entity_name.

  ENDIF.

ENDMETHOD.


METHOD employeedataset_get_entityset.

* Work area declaration
  DATA: ls_paging     TYPE /iwbep/s_mgw_paging,
        ls_sort_order TYPE /iwbep/s_mgw_sorting_order.

* Internal Table/Range Table declaration
  DATA: lt_filter_select TYPE /iwbep/t_mgw_select_option,
        lt_techorder     TYPE /iwbep/t_mgw_tech_order,
        lt_sort_order    TYPE /iwbep/t_mgw_sorting_order,
        lt_return        TYPE bapiret2_t,                  " return message
        lt_error         TYPE bapiret2_t,
        lr_pernr         TYPE /iwbep/t_cod_select_options,
        lr_ename         TYPE /iwbep/t_cod_select_options,
        lr_bukrs         TYPE /iwbep/t_cod_select_options,
        lr_werks         TYPE /iwbep/t_cod_select_options,
        lr_persg         TYPE /iwbep/t_cod_select_options,
        lr_persk         TYPE /iwbep/t_cod_select_options,
        lr_btrtl         TYPE /iwbep/t_cod_select_options,
        lr_gesch         TYPE /iwbep/t_cod_select_options,
        lr_land1         TYPE /iwbep/t_cod_select_options,
        lr_bukrs_new     TYPE /iwbep/t_cod_select_options,
        lr_gesch_new     TYPE /iwbep/t_cod_select_options,
        lr_land1_new     TYPE /iwbep/t_cod_select_options,
        lr_location      TYPE /iwbep/t_cod_select_options.

* Class object declaration
  DATA: lo_filter  TYPE  REF TO /iwbep/if_mgw_req_filter,
        lo_objname TYPE REF TO /clin/cl_hcm_anno_ref,
        lo_meco    TYPE REF TO /iwbep/if_message_container.  " message container

* Local Variable declaration
  DATA: lv_filter_str       TYPE string.

* Field Symbol  declaration
  FIELD-SYMBOLS: <lfs_return> TYPE bapiret2,                    " return message
                 <lfs_order>  TYPE /iwbep/s_mgw_tech_order,
                 <lfs_filter> TYPE /iwbep/s_mgw_select_option.

* Constant  declaration
  CONSTANTS: lc_msgty     TYPE symsgty    VALUE 'E',
             lc_msgid     TYPE symsgid    VALUE '/IWBEP/MC_SB_DPC_ADM',
             lc_msgtyp    TYPE cc_string  VALUE 'EA',
             lc_pernr     TYPE cc_string VALUE  'PERNR',
             lc_ename     TYPE cc_string VALUE  'ENAME',
             lc_bukrs     TYPE cc_string VALUE  'BUKRS',
             lc_werks     TYPE cc_string VALUE  'WERKS',
             lc_persg     TYPE cc_string VALUE  'PERSG',
             lc_persk     TYPE cc_string VALUE  'PERSK',
             lc_btrtl     TYPE cc_string VALUE  'BTRTL',
             lc_gesch     TYPE cc_string VALUE  'GESCH',
             lc_land1     TYPE cc_string VALUE  'LAND1',
             lc_bukrs_new TYPE cc_string VALUE  'BUKRS_NEW',
             lc_gesch_new TYPE cc_string VALUE  'GESCH_NEW',
             lc_land1_new TYPE cc_string VALUE  'LAND1_NEW',
             	lc_location	 TYPE cc_string VALUE	'LOCATION'.

  CLEAR et_entityset.
*-------------------------------------------------------------------------*
*           Get FILTER or select option information
*-------------------------------------------------------------------------*
* Get Filter object
  lo_filter = io_tech_request_context->get_filter( ).

  IF lo_filter  IS BOUND.
    lt_filter_select = lo_filter->get_filter_select_options( ).
    lv_filter_str    = lo_filter->get_filter_string( ).

*   Check if the filter is supported by standard gateway runtime process
    IF  lv_filter_str            IS NOT INITIAL
    AND lt_filter_select IS INITIAL.

*     If the string of the Filter System Query Option is not automatically converted into
*     filter option table (lt_filter_select_options), then the filtering combination is not supported
*     Log message in the application log
      /iwbep/if_sb_dpc_comm_services~log_message(
        EXPORTING
          iv_msg_type   = lc_msgty
          iv_msg_id     = lc_msgid
          iv_msg_number = 020
          iv_msg_v1     = iv_entity_name ).
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
        EXPORTING
          textid = /iwbep/cx_mgw_tech_exception=>internal_error.
    ENDIF.

    LOOP AT lt_filter_select ASSIGNING <lfs_filter>.

      CASE <lfs_filter>-property.
        WHEN lc_pernr .
          lo_filter->convert_select_option(
                 EXPORTING
                   is_select_option = <lfs_filter>
                 IMPORTING
                   et_select_option = lr_pernr ).

        WHEN lc_ename .
          lo_filter->convert_select_option(
                 EXPORTING
                   is_select_option = <lfs_filter>
                 IMPORTING
                   et_select_option = lr_ename ).

        WHEN lc_bukrs .
          lo_filter->convert_select_option(
                 EXPORTING
                   is_select_option = <lfs_filter>
                 IMPORTING
                   et_select_option = lr_bukrs ).

        WHEN lc_werks .
          lo_filter->convert_select_option(
                 EXPORTING
                   is_select_option = <lfs_filter>
                 IMPORTING
                   et_select_option = lr_werks ).

        WHEN lc_persg .
          lo_filter->convert_select_option(
                 EXPORTING
                   is_select_option = <lfs_filter>
                 IMPORTING
                   et_select_option = lr_persg ).

        WHEN lc_persk .
          lo_filter->convert_select_option(
                 EXPORTING
                   is_select_option = <lfs_filter>
                 IMPORTING
                   et_select_option = lr_persk ).

        WHEN lc_btrtl .
          lo_filter->convert_select_option(
                 EXPORTING
                   is_select_option = <lfs_filter>
                 IMPORTING
                   et_select_option = lr_btrtl ).

        WHEN lc_gesch .
          lo_filter->convert_select_option(
                 EXPORTING
                   is_select_option = <lfs_filter>
                 IMPORTING
                   et_select_option = lr_gesch ).

        WHEN lc_land1 .
          lo_filter->convert_select_option(
                 EXPORTING
                   is_select_option = <lfs_filter>
                 IMPORTING
                   et_select_option = lr_land1 ).

        WHEN lc_bukrs_new .
          lo_filter->convert_select_option(
                 EXPORTING
                   is_select_option = <lfs_filter>
                 IMPORTING
                   et_select_option = lr_bukrs_new ).

        WHEN lc_gesch_new .
          lo_filter->convert_select_option(
                 EXPORTING
                   is_select_option = <lfs_filter>
                 IMPORTING
                   et_select_option = lr_gesch_new ).

        WHEN lc_land1_new .
          lo_filter->convert_select_option(
                 EXPORTING
                   is_select_option = <lfs_filter>
                 IMPORTING
                   et_select_option = lr_land1_new ).

        WHEN lc_location .
          lo_filter->convert_select_option(
                 EXPORTING
                   is_select_option = <lfs_filter>
                 IMPORTING
                   et_select_option = lr_location ).

        WHEN OTHERS.
          /iwbep/if_sb_dpc_comm_services~log_message(
       EXPORTING
         iv_msg_type   = lc_msgty
         iv_msg_id     = lc_msgid
         iv_msg_number = 020
         iv_msg_v1     = iv_entity_name ).
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
            EXPORTING
              textid = /iwbep/cx_mgw_tech_exception=>internal_error.
      ENDCASE.
    ENDLOOP.
  ENDIF.


*-------------------------------------------------------------------------*
*  Call Business class and error handling. Resultset of business
*  IMPORTING parameter of business class should be assgined to ET_ENTITYSET
*-------------------------------------------------------------------------*
* Sample Code here

  IF lo_objname IS NOT BOUND.
    CREATE OBJECT lo_objname.
  ENDIF.

  IF lo_objname IS BOUND.
    CALL METHOD lo_objname->get_entityset
      EXPORTING
        ir_pernr     = lr_pernr
        ir_ename     = lr_ename
        ir_bukrs     = lr_bukrs
        ir_werks     = lr_werks
        ir_persg     = lr_persg
        ir_persk     = lr_persk
        ir_btrtl     = lr_btrtl
        ir_gesch     = lr_gesch
        ir_land1     = lr_land1
        ir_bukrs_new = lr_bukrs_new
        ir_gesch_new = lr_gesch_new
        ir_land1_new = lr_land1_new
        ir_location  = lr_location
      IMPORTING
        et_output    = et_entityset            "Current EntiitySet
        et_return    = lt_return.
  ENDIF.
*-------------------------------------------------------------------------*
* check return code
  LOOP AT lt_return ASSIGNING <lfs_return>               "#EC CI_STDSEQ
* less than 500 lines
  WHERE type CA lc_msgtyp.
    INSERT <lfs_return> INTO TABLE lt_error.
  ENDLOOP.

  IF lt_error IS NOT INITIAL.

    " check that mo_context is already bound before it is being used later
    IF mo_context IS NOT BOUND.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
        EXPORTING
          textid = /iwbep/cx_mgw_tech_exception=>internal_error.
    ENDIF.

    lo_meco = mo_context->get_message_container( ).

    IF lo_meco IS BOUND.
      lo_meco->add_messages_from_bapi( it_bapi_messages = lt_error ).
    ENDIF.

    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_meco
        textid            = /iwbep/cx_mgw_busi_exception=>business_error_unlimited
        message_unlimited = iv_entity_name.

  ENDIF.

*-------------------------------------------------------------------------*
*           Get ORDER BY or SORT data
*-------------------------------------------------------------------------*
  lt_techorder     = io_tech_request_context->get_orderby( ).

  LOOP AT lt_techorder ASSIGNING <lfs_order>.
    ls_sort_order-order = <lfs_order>-order.
    ls_sort_order-property = <lfs_order>-property.
    APPEND ls_sort_order TO lt_sort_order.
    CLEAR ls_sort_order.
  ENDLOOP.
* Sort data based sorted criteria provided by query string
  IF lt_sort_order IS NOT INITIAL.
    /iwbep/cl_mgw_data_util=>orderby( EXPORTING it_order = lt_sort_order CHANGING ct_data = et_entityset ).
  ENDIF.

*-------------------------------------------------------------------------*
*             INLINE COUNT
*-------------------------------------------------------------------------*

  IF io_tech_request_context->has_inlinecount( ) = abap_true.
    DESCRIBE TABLE et_entityset LINES es_response_context-inlinecount.
  ELSE.
    CLEAR es_response_context-inlinecount.
  ENDIF.

*-------------------------------------------------------------------------*
*             PAGINATION (TOP/SKIP)
*-------------------------------------------------------------------------*

*     get number of records requested
  ls_paging-top = io_tech_request_context->get_top( ).
*     get number of lines that should be skipped
  ls_paging-skip = io_tech_request_context->get_skip( ).

  /iwbep/cl_mgw_data_util=>paging( EXPORTING is_paging = ls_paging CHANGING ct_data = et_entityset ).


ENDMETHOD.


METHOD employeedataset_update_entity.

* Work Area declaration
  DATA: ls_input_data    TYPE /clin/cl_hcm_anno_ref_mpc=>ts_employeedata.

* Internal Table/Range Table declaration
  DATA: lt_return TYPE bapiret2_t,                  " return message
        lt_error  TYPE bapiret2_t,                  " return message
        lt_keys   TYPE /iwbep/t_mgw_tech_pairs.

* Class Object Declaration
  DATA: lo_meco    TYPE REF TO /iwbep/if_message_container,  " message container
        lo_objname TYPE REF TO /clin/cl_hcm_anno_ref.

* Local variable declaration
  DATA: lv_pernr            TYPE  /clin/cl_hcm_anno_ref_mpc=>ts_employeedata-pernr.

* Field Symbol  declaration
  FIELD-SYMBOLS: <lfs_return> TYPE bapiret2,                    " return message
                 <lfs_keys>   TYPE /iwbep/s_mgw_tech_pair.

* Constant  declaration
  CONSTANTS:lc_msgtyp TYPE cc_string  VALUE 'EA',
            lc_pernr  TYPE cc_string VALUE    'PERNR'.

  CLEAR : er_entity.

* Read Request Data
  IF io_data_provider IS BOUND.
    io_data_provider->read_entry_data( IMPORTING es_data = ls_input_data ).
    er_entity = ls_input_data.
  ENDIF.

  lt_keys = io_tech_request_context->get_keys( ).
  "Get Key values
  READ TABLE lt_keys ASSIGNING <lfs_keys> WITH KEY name = lc_pernr.
  IF sy-subrc = 0.
    lv_pernr = <lfs_keys>-value.
  ENDIF.

**********************************************************************
*Add Custom class logic here

  IF lo_objname IS NOT BOUND.
    CREATE OBJECT lo_objname.
  ENDIF.

  IF lo_objname IS BOUND.
    CALL METHOD lo_objname->update_entity
      EXPORTING
        is_input  = ls_input_data         "Input param Description
      IMPORTING
        er_entity = er_entity             "Current Entiity Set
        et_return = lt_return.
  ENDIF.
**********************************************************************

  " check return code
  LOOP AT lt_return ASSIGNING <lfs_return>               "#EC CI_STDSEQ
  " less than 500 lines
  WHERE type CA lc_msgtyp.
    INSERT <lfs_return> INTO TABLE lt_error.
  ENDLOOP.

  IF lt_error IS NOT INITIAL.

    " check that mo_context is already bound before it is being used later
    IF mo_context IS NOT BOUND.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
        EXPORTING
          textid = /iwbep/cx_mgw_tech_exception=>internal_error.
    ENDIF.

    lo_meco = mo_context->get_message_container( ).

    IF lo_meco IS BOUND.
      lo_meco->add_messages_from_bapi( it_bapi_messages = lt_error ).
    ENDIF.

    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_meco
        textid            = /iwbep/cx_mgw_busi_exception=>business_error_unlimited
        message_unlimited = iv_entity_name.

  ENDIF.


ENDMETHOD.
ENDCLASS.
