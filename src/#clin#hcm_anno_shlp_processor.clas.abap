CLASS /clin/hcm_anno_shlp_processor DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_entityset
      IMPORTING
        !io_tech_request_context TYPE REF TO /iwbep/if_mgw_req_entityset
        !ir_request_details      TYPE REF TO /iwbep/if_mgw_core_srv_runtime=>ty_s_mgw_request_context
      EXPORTING
        !er_entityset            TYPE REF TO data
        !es_response_context     TYPE /iwbep/if_mgw_appl_srv_runtime=>ty_s_mgw_response_context
        !ev_done                 TYPE abap_bool
      RAISING
        /iwbep/cx_mgw_busi_exception
        /iwbep/cx_mgw_tech_exception .
    CLASS-METHODS get_domain_values
      IMPORTING
        !iv_domain               TYPE domname
        !io_tech_request_cntxt   TYPE REF TO /iwbep/if_mgw_req_entity OPTIONAL
        !io_tech_request_context TYPE REF TO /iwbep/if_mgw_req_entityset OPTIONAL
      EXPORTING
        !et_entityset            TYPE STANDARD TABLE
        !es_response_context     TYPE /iwbep/if_mgw_appl_srv_runtime=>ty_s_mgw_response_context
      RAISING
        /iwbep/cx_mgw_tech_exception
        /iwbep/cx_mgw_busi_exception .
    CLASS-METHODS get_entity
      IMPORTING
        !io_tech_request_context TYPE REF TO /iwbep/if_mgw_req_entity OPTIONAL
        !ir_request_details      TYPE REF TO /iwbep/if_mgw_core_srv_runtime=>ty_s_mgw_request_context
      EXPORTING
        !ev_done                 TYPE abap_bool
        !er_entity               TYPE REF TO data
      RAISING
        /iwbep/cx_mgw_busi_exception
        /iwbep/cx_mgw_tech_exception .
    CLASS-METHODS get_values
      IMPORTING
        !iv_search_help          TYPE shlpname
        !iv_search_help_field    TYPE fieldname
        !io_tech_request_context TYPE REF TO /iwbep/if_mgw_req_entityset
      EXPORTING
        !er_entity               TYPE REF TO data
        !et_entityset            TYPE ANY TABLE
        !es_response_context     TYPE /iwbep/if_mgw_appl_srv_runtime=>ty_s_mgw_response_context
      RAISING
        /iwbep/cx_mgw_tech_exception .
    CLASS-METHODS get_focus
      IMPORTING
        !ir_request_details TYPE REF TO /iwbep/if_mgw_core_srv_runtime=>ty_s_mgw_request_context
      RETURNING
        VALUE(rv_focus)     TYPE string .
  PROTECTED SECTION.
private section.

  class-data GO_MSG_CONTAINER type ref to /IWBEP/IF_MESSAGE_CONTAINER .

  class-methods DDIC_NAME
    importing
      !IV_ODATA_NAME type CSEQUENCE
    returning
      value(RV_DDIC_NAME) type SHLPNAME .
  class-methods CHECK_ORDERBY
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  class-methods CREATE_DYNAMIC_STRUCTURE
    importing
      !IT_FIELDS type DDFIELDS
    returning
      value(RO_DATA) type ref to DATA
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  class-methods BUILD_WHERE_CLAUSE_FROM_FILTER
    importing
      !IV_TABNAME type CHAR30 optional
      !IV_ATTRIBUTE_NAME type CHAR30 optional
      !IS_FILTER_SELECT_OPTION type /IWBEP/S_COD_SELECT_OPTION
    exporting
      !ET_FIELD_RANGES type RSDS_TRANGE .
  class-methods BUILD_SELOPT_FROM_KEY
    importing
      !IT_KEY_TAB type /IWBEP/T_MGW_TECH_PAIRS
    exporting
      !ET_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION .
  class-methods ADD_TABLE_2_WHERE_STRING
    importing
      !IV_CHECKTABLE type DDOBJNAME
      !IV_TEXTTABLE type DDOBJNAME
    changing
      !CV_FILTER_STRING type STRING .
  class-methods BUILD_FILTER_STRING_FROM_KEY
    importing
      !IT_KEY_TAB type /IWBEP/T_MGW_TECH_PAIRS
    changing
      !CV_FILTER_STRING type STRING .
  class-methods CREATE_DYNAMIC_TABLE
    importing
      !IT_FIELDS type DDFIELDS
    returning
      value(RO_DATA) type ref to DATA
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  class-methods RAISE_TECHNICAL_EXCEPTION
    importing
      !IV_HTTP_STATUS_CODE type /IWBEP/MGW_HTTP_STATUS_CODE
      !IX_PREVIOUS type ref to CX_ROOT optional
      !IV_ADD_LEADING_MSG type ABAP_BOOL default ABAP_FALSE
      !IV_MSG_TYPE type SYMSGTY default SY-MSGTY
      !IV_MSG_ID type SYMSGID default SY-MSGID
      !IV_MSG_NUMBER type SYMSGNO default SY-MSGNO
      !IV_MSG_V1 type SYMSGV default SY-MSGV1
      !IV_MSG_V2 type SYMSGV default SY-MSGV2
      !IV_MSG_V3 type SYMSGV default SY-MSGV3
      !IV_MSG_V4 type SYMSGV default SY-MSGV4
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  class-methods CREATE_STRUCT_FROM_FIELDS
    importing
      !IT_FIELDS type DDFIELDS
    returning
      value(RO_DATA) type ref to DATA
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  class-methods GET_SELECT_CLAUSE_CT
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
      !I_CHECKTAB type SELMETHOD
      !I_TEXTTAB type SELMETHTXT optional
      !I_INTDESCR type SHLP_INTDS optional
      !IT_FIELDDESCR type DDFIELDS optional
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION optional
    exporting
      !ET_ENTITYSET type STANDARD TABLE
      !ET_WHERE_CLAUSES type RSDS_TWHERE
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
      !EV_DBTAB_SYNTAX type STRING
      !EV_SELECT type STRING
    changing
      !RV_WHERE_CLAUSE type STRING
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  class-methods CREATE_TAB_FROM_FIELDS
    importing
      !IT_FIELDS type DDFIELDS
    returning
      value(RO_DATA) type ref to DATA
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  class-methods GET_VALUES_NO_TYPE_AHEAD
    importing
      !IV_SEARCH_HELP type SHLPNAME
      !IV_SEARCH_HELP_FIELD type FIELDNAME
      !IO_TECH_REQUEST_CNTXT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
      !IV_RAW_FILTER type STRING optional
    exporting
      !ER_ENTITY type ref to DATA
      !ER_ENTITYSET type ref to DATA
      !ET_ENTITYSET type ANY TABLE
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  class-methods DETERMINE_COMPONENT_TYPE
    importing
      !ID_INTTYPE type INTTYPE
      !ID_LENGTH type I
      !ID_INTLENGTH type I
      !ID_DECIMALS type I
    returning
      value(RO_COMPONENT_TYPE) type ref to CL_ABAP_DATADESCR
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION .
ENDCLASS.



CLASS /CLIN/HCM_ANNO_SHLP_PROCESSOR IMPLEMENTATION.


  METHOD add_table_2_where_string.

    DATA: lt_ddfields        TYPE ddfields,
          lt_ddfields_t      TYPE ddfields,
          lv_fieldname       TYPE string,
          lv_fieldname_tilde TYPE string.

    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        tabname   = iv_checktable
        all_types = 'X'
      TABLES
        dfies_tab = lt_ddfields
      EXCEPTIONS
        not_found = 1
        OTHERS    = 99.

    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        tabname   = iv_texttable
        all_types = 'X'
      TABLES
        dfies_tab = lt_ddfields_t
      EXCEPTIONS
        not_found = 1
        OTHERS    = 99.

*-- replace checktable fieldname with a~fieldname in where string
    LOOP AT lt_ddfields INTO DATA(ls_ddfields).
      lv_fieldname = | | &&  ls_ddfields-fieldname && | |.
      lv_fieldname_tilde = | a~| &&  ls_ddfields-fieldname && | |.
      REPLACE ALL OCCURRENCES OF lv_fieldname IN cv_filter_string WITH lv_fieldname_tilde.
    ENDLOOP.

*-- replace texttable fieldname with b~fieldname in where string
    LOOP AT lt_ddfields_t INTO DATA(ls_ddfields_t).
      lv_fieldname = | | &&  ls_ddfields_t-fieldname && | |.
      lv_fieldname_tilde = | b~| &&  ls_ddfields_t-fieldname && | |.
      REPLACE ALL OCCURRENCES OF lv_fieldname IN cv_filter_string WITH lv_fieldname_tilde.
    ENDLOOP.

  ENDMETHOD.


  METHOD build_filter_string_from_key.

    LOOP AT it_key_tab INTO DATA(ls_key).

      IF cv_filter_string IS NOT INITIAL.
        cv_filter_string = cv_filter_string && | and |.
      ENDIF.
      ls_key-value = cl_abap_dyn_prg=>escape_quotes( ls_key-value ).

      cv_filter_string = cv_filter_string  && | a~| && |{ ls_key-name } = '{ ls_key-value }'|.
    ENDLOOP.

  ENDMETHOD.


  METHOD build_selopt_from_key.

    DATA: ls_select_option TYPE /iwbep/s_mgw_select_option,
          ls_selopt        TYPE /iwbep/s_cod_select_option.

    LOOP AT it_key_tab INTO DATA(ls_key).
      ls_select_option-property = ls_key-name.
      ls_selopt-option = 'EQ'.
      ls_selopt-sign = 'I'.
      ls_selopt-low = cl_abap_dyn_prg=>escape_quotes( ls_key-value ).
      REPLACE ALL OCCURRENCES OF '*' IN ls_selopt-low WITH '%'.
      APPEND ls_selopt TO ls_select_option-select_options.
      APPEND ls_select_option TO et_select_options.
    ENDLOOP.

  ENDMETHOD.


  METHOD build_where_clause_from_filter.
    DATA:
      ls_frange       TYPE rsds_frange,
      ls_sel_opt      TYPE rsdsselopt,
      ls_field_range  TYPE rsds_range,
      lt_field_ranges TYPE rsds_trange,
      ls_rsdsselopt   TYPE rsdsselopt.

    ls_field_range-tablename = iv_tabname.
    ls_frange-fieldname      = iv_attribute_name.
    MOVE-CORRESPONDING is_filter_select_option TO ls_rsdsselopt.
    APPEND:  ls_rsdsselopt TO ls_frange-selopt_t,
             ls_frange TO ls_field_range-frange_t.

    CHECK ls_field_range-frange_t IS NOT INITIAL.
    APPEND ls_field_range TO et_field_ranges.

  ENDMETHOD.


  METHOD check_orderby.
    CHECK lines( io_tech_request_context->get_orderby( ) ) > 0.

    " $orderby not possible, type-ahead processor does its own sorting

    " TODO: reactiveate check after finding out how to avoid $orderby on the client
*    raise exception type /iwbep/cx_mgw_tech_exception
*      exporting
*        textid           = /iwbep/cx_mgw_tech_exception=>operation_not_supported
*        http_status_code = /iwbep/cx_mgw_tech_exception=>gcs_http_status_codes-not_implemented
*        operation        = '$orderby' "#EC NOTEXT
*        entity_type      = io_tech_request_context->get_entity_type_name( ).

  ENDMETHOD.


  METHOD create_dynamic_structure.
    DATA: ls_shlpfld TYPE dfies,
          lt_fields  TYPE STANDARD TABLE OF dfies,
          ls_field   TYPE dfies.

    FIELD-SYMBOLS: <keytab> TYPE table,
                   <keylin> TYPE any,
                   <sellin> TYPE any,
                   <shplin> TYPE any.

*create the search help field data structure
    LOOP AT it_fields INTO ls_shlpfld.
      CLEAR ls_field.
      MOVE-CORRESPONDING ls_shlpfld TO ls_field.
      ls_field-intlen = ls_shlpfld-intlen.
      ls_field-leng = ls_shlpfld-leng.
      ls_field-inttype = ls_shlpfld-inttype.
      APPEND ls_field TO lt_fields.
    ENDLOOP.
    ro_data = create_struct_from_fields( lt_fields ).

  ENDMETHOD.


  METHOD create_dynamic_table.
    DATA: ls_shlpfld TYPE dfies,
          lt_fields  TYPE STANDARD TABLE OF dfies,
          ls_field   TYPE dfies,
          ls_rec     TYPE seahlpres,
          ld_idx     TYPE i.

    FIELD-SYMBOLS: <keytab> TYPE table,
                   <keylin> TYPE any,
                   <sellin> TYPE any,
                   <shplin> TYPE any.

*create the search help field data structure
    LOOP AT it_fields INTO ls_shlpfld.
      CLEAR ls_field.
      MOVE-CORRESPONDING ls_shlpfld TO ls_field.
      ls_field-intlen = ls_shlpfld-intlen.
      ls_field-leng = ls_shlpfld-leng.
      ls_field-inttype = ls_shlpfld-inttype.
      APPEND ls_field TO lt_fields.
    ENDLOOP.
    ro_data = create_tab_from_fields( lt_fields ).

  ENDMETHOD.


  METHOD create_struct_from_fields.

    DATA: lt_comps     TYPE cl_abap_structdescr=>component_table,
          lo_strds     TYPE REF TO cl_abap_structdescr,
          lo_tabds     TYPE REF TO cl_abap_tabledescr,
          ls_field     TYPE dfies,
          ls_component TYPE cl_abap_structdescr=>component,
          ld_len       TYPE i,
          ld_intlen    TYPE i,
          ld_decimals  TYPE i.

*initialise
*  CLEAR: rt_components.

*process all the fields and determine the components
    LOOP AT it_fields INTO ls_field.

*.setup the parameters
      ld_len = ls_field-leng.
      ld_intlen = ls_field-intlen.
      ld_decimals = ls_field-decimals.

*.setup the component name & type
      CLEAR ls_component.
      ls_component-name = ls_field-fieldname.
      ls_component-type = determine_component_type( id_inttype      = ls_field-inttype
                                                    id_length       = ld_len
                                                    id_intlength    = ld_intlen
                                                    id_decimals     = ld_decimals ).
*append to the component table
      IF NOT ls_component-type IS INITIAL.
        APPEND ls_component TO lt_comps.
      ENDIF.

    ENDLOOP.

*create structure type
    TRY.
        lo_strds = cl_abap_structdescr=>create( lt_comps ).
*             catch cx_sy_table_creation.  "
*.exception handling: couldn't create structure type
    ENDTRY.

*create the data reference
    TRY.
        CREATE DATA ro_data TYPE HANDLE lo_strds.
*.exception handling, error creating data
    ENDTRY.

  ENDMETHOD.


  METHOD create_tab_from_fields.

    DATA: lt_comps     TYPE cl_abap_structdescr=>component_table,
          lo_strds     TYPE REF TO cl_abap_structdescr,
          lo_tabds     TYPE REF TO cl_abap_tabledescr,
          ls_field     TYPE dfies,
          ls_component TYPE cl_abap_structdescr=>component,
          ld_len       TYPE i,
          ld_intlen    TYPE i,
          ld_decimals  TYPE i.

*initialise
*  CLEAR: rt_components.

*process all the fields and determine the components
    LOOP AT it_fields INTO ls_field.

*.setup the parameters
      ld_len = ls_field-leng.
      ld_intlen = ls_field-intlen.
      ld_decimals = ls_field-decimals.

*.setup the component name & type
      CLEAR ls_component.
      ls_component-name = ls_field-fieldname.
      ls_component-type = determine_component_type( id_inttype      = ls_field-inttype
                                                    id_length       = ld_len
                                                    id_intlength    = ld_intlen
                                                    id_decimals     = ld_decimals ).
*append to the component table
      IF NOT ls_component-type IS INITIAL.
        APPEND ls_component TO lt_comps.
      ENDIF.

    ENDLOOP.

*create structure type
    TRY.
        lo_strds = cl_abap_structdescr=>create( lt_comps ).
        lo_tabds = cl_abap_tabledescr=>create(
                                 p_line_type = lo_strds           ).
*             catch cx_sy_table_creation.  "
*.exception handling: couldn't create structure type
    ENDTRY.

*create the data reference
    TRY.
        CREATE DATA ro_data TYPE HANDLE lo_tabds.
*.exception handling, error creating data
    ENDTRY.

  ENDMETHOD.


  METHOD ddic_name.   "JCN
    rv_ddic_name = iv_odata_name.
    TRANSLATE rv_ddic_name USING 'x/'.
  ENDMETHOD.


  METHOD determine_component_type.
*initialise
    CLEAR ro_component_type.

*depending on the internal type, build the RTTS component type
    CASE id_inttype.
      WHEN 'C'.
        ro_component_type = cl_abap_elemdescr=>get_c( id_length ).
      WHEN 'D'.
        ro_component_type = cl_abap_elemdescr=>get_d( ).
      WHEN 'F'.
        ro_component_type = cl_abap_elemdescr=>get_f( ).
      WHEN 'I'.
        ro_component_type = cl_abap_elemdescr=>get_i( ).
      WHEN 'N'.
        ro_component_type = cl_abap_elemdescr=>get_n( id_length ).
      WHEN 'P'.
        ro_component_type = cl_abap_elemdescr=>get_p( p_length = id_intlength
                                                      p_decimals = id_decimals ).
      WHEN 'g'.
        ro_component_type = cl_abap_elemdescr=>get_string( ).
      WHEN 'T'.
        ro_component_type = cl_abap_elemdescr=>get_t( ).
      WHEN 'X'.
        ro_component_type = cl_abap_elemdescr=>get_x( id_intlength ).
      WHEN 'Y'.
        ro_component_type = cl_abap_elemdescr=>get_xstring( ).
      WHEN space.
      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.


  METHOD get_domain_values.
    DATA:
      lv_top     TYPE i,
      lv_skip    TYPE i,
      lv_rows    TYPE i,
      lv_domname TYPE domname,
      lt_sort    TYPE abap_sortorder_tab,
      ls_sort    LIKE LINE OF lt_sort,
      lt_dd07v   TYPE STANDARD TABLE OF dd07v.

    DATA(lv_where) = |DOMNAME = '{ cl_abap_dyn_prg=>escape_quotes( iv_domain ) }' and DDLANGUAGE = SY-LANGU|.
    IF io_tech_request_context IS BOUND.
      DATA(lv_filter) = io_tech_request_context->get_osql_where_clause( ).
      IF lv_filter IS NOT INITIAL.
        lv_where = lv_where && ` and ` && lv_filter.
      ENDIF.

      " TODO: generate text index on replacement for dd07v and use contains(*,...)
      DATA(lv_search) = cl_abap_dyn_prg=>escape_quotes( io_tech_request_context->get_search_string( ) ).
      IF lv_search IS NOT INITIAL.
        lv_where = lv_where && | and ( DOMVALUE_L like '%{ lv_search }%' or DDTEXT like '%{ lv_search }%' )|.
      ENDIF.

      lv_top = io_tech_request_context->get_top( ).
      IF lv_top > 0.
        lv_skip = io_tech_request_context->get_skip( ).
        lv_rows = lv_skip + lv_top.
      ENDIF.

      check_orderby( io_tech_request_context ).
    ENDIF.

    lv_domname = cl_abap_dyn_prg=>escape_quotes( iv_domain ).
    ls_sort-name = 'domvalue_l'.
    ls_sort-descending = abap_false.
    APPEND ls_sort TO lt_sort.

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname   = lv_domname
        text      = 'T'
        langu     = sy-langu
      TABLES
        dd07v_tab = lt_dd07v.

    IF lt_dd07v IS INITIAL.
      CALL FUNCTION 'DD_DOMVALUES_GET'
        EXPORTING
          domname   = lv_domname
          text      = 'X'
          langu     = 'E'  "retrieve the english texts
        TABLES
          dd07v_tab = lt_dd07v.
    ENDIF.

    IF lv_top > 0.
      ADD 1 TO lv_rows.
      DELETE lt_dd07v FROM lv_rows.                     "#EC CI_NOORDER
    ENDIF.

    SORT lt_dd07v BY (lt_sort).
    MOVE-CORRESPONDING lt_dd07v TO et_entityset.
*    IF 1 = 2. " Old selection
*      " TODO: replace dd07v with correct language-join of dd07l and dd07t via t002
*      SELECT domvalue_l ddtext INTO CORRESPONDING FIELDS OF TABLE et_entityset UP TO lv_rows ROWS
*        FROM dd07v
*       WHERE (lv_where)
*       ORDER BY domvalue_l.
*    ENDIF.
*    es_response_context-inlinecount = lines( et_entityset ).
*    if es_response_context-inlinecount = lv_rows.
*      " assume there is at least one more matching entry beyond lv_rows
*      add 1 to es_response_context-inlinecount.
*    endif.

    IF lv_skip > 0.
      DELETE et_entityset TO lv_skip.                   "#EC CI_NOORDER
    ENDIF.

  ENDMETHOD.


  METHOD get_entity.
    DATA:
      lv_entity_type_name  TYPE /iwbep/mgw_tech_name,
      lr_key_tab           TYPE REF TO /iwbep/s_mgw_name_value_pair,
      lv_domain            TYPE domname,
      ls_keys              TYPE REF TO data, "LIKE er_entity,
      lv_search_help       TYPE shlpname,
      lv_search_help_field TYPE fieldname,
      lv_search_string     TYPE string,
      ls_search_help       TYPE shlp_descr_t,
      lv_where_clause      TYPE string,
      ls_where             TYPE rsds_where,
      lt_where             TYPE rsds_where_tab,
      lv_filter_string     TYPE string,
      lv_dbtab_syntax      TYPE string,
      lv_select            TYPE string,
      lt_where_clauses     TYPE STANDARD TABLE OF rsds_where,
      lt_entityset         TYPE REF TO data,
      lt_keys              TYPE /iwbep/t_mgw_tech_pairs,
      lt_selopt            TYPE  /iwbep/t_mgw_select_option,
      lv_tab               TYPE string
      .

    FIELD-SYMBOLS:
      <values> TYPE STANDARD TABLE.

    lv_entity_type_name = io_tech_request_context->get_entity_type_name( ).

    IF lv_entity_type_name(6) = /clin/hcm_anno_shlp_annotation=>gc_fv.
      ev_done = abap_true.

      lv_domain = ddic_name( lv_entity_type_name+6 ).

      CREATE DATA er_entity TYPE STANDARD TABLE OF dd07v.
      ASSIGN er_entity->* TO <values>.

      /clin/hcm_anno_shlp_processor=>get_domain_values(
        EXPORTING
          iv_domain               = lv_domain
          io_tech_request_cntxt   = io_tech_request_context
        IMPORTING
          et_entityset            = <values> ).
      er_entity = REF #( <values>[ 1 ] ).

    ELSEIF lv_entity_type_name(6) = /clin/hcm_anno_shlp_annotation=>gc_sh.
      ev_done = abap_true.

      lv_search_help       = ddic_name( lv_entity_type_name+6 ).
      lv_search_help_field = /clin/hcm_anno_shlp_processor=>get_focus( ir_request_details ).

      /clin/hcm_anno_shlp_processor=>get_values_no_type_ahead(
        EXPORTING
          iv_search_help          = lv_search_help
          iv_search_help_field    = lv_search_help_field
          io_tech_request_cntxt   = io_tech_request_context
        IMPORTING
          er_entity               = er_entity ).

    ELSEIF lv_entity_type_name(6) = /clin/hcm_anno_shlp_annotation=>gc_ch
        OR lv_entity_type_name(6) = /clin/hcm_anno_shlp_annotation=>gc_ct.
      ev_done = abap_true.

      ls_search_help-shlpname = lv_entity_type_name+6.
*      ls_search_help-shlptype = lv_entity_type_name+3(2).
      ls_search_help-shlptype = /clin/hcm_anno_shlp_annotation=>gc_ch+3(2).
*      When shlptype is 'CT', Function Module DD_SHLP_GET_HELPMETHOD must be called with type 'CH' (internal message 748242 2014)

      CALL FUNCTION 'DD_SHLP_GET_HELPMETHOD'
        CHANGING
          shlp = ls_search_help.

      CALL METHOD /clin/hcm_anno_shlp_processor=>build_selopt_from_key
        EXPORTING
          it_key_tab        = io_tech_request_context->get_keys( )
        IMPORTING
          et_select_options = lt_selopt.

      build_filter_string_from_key(
        EXPORTING
          it_key_tab          = io_tech_request_context->get_keys( )
        CHANGING
          cv_filter_string    = lv_where_clause ).

      lt_entityset = create_dynamic_table( it_fields = ls_search_help-fielddescr ).
      ASSIGN lt_entityset->* TO <values>.

      IF lv_entity_type_name(6) = /clin/hcm_anno_shlp_annotation=>gc_ch.
        ls_search_help-intdescr-selmethod = cl_abap_dyn_prg=>escape_quotes( ls_search_help-intdescr-selmethod ).
        lv_tab = ls_search_help-intdescr-selmethod.
        TRY.
            ls_search_help-intdescr-selmethod = cl_abap_dyn_prg=>check_whitelist_str(
                 val       = ls_search_help-intdescr-selmethod
                 whitelist = lv_tab
                    ).
          CATCH cx_abap_not_in_whitelist .
        ENDTRY.
        REPLACE ALL OCCURRENCES OF '´' IN lv_where_clause WITH '@*>'.
        lv_where_clause = cl_abap_dyn_prg=>escape_quotes_str( val = lv_where_clause ).
        REPLACE ALL OCCURRENCES OF '@*>' IN lv_where_clause WITH '´'.
        SELECT * FROM (ls_search_help-intdescr-selmethod) AS a INTO CORRESPONDING FIELDS OF TABLE <values>
         WHERE (lv_where_clause).
      ELSE.
        TRY.
            CALL METHOD /clin/hcm_anno_shlp_processor=>get_select_clause_ct
              EXPORTING
                i_checktab               = ls_search_help-intdescr-selmethod
                i_intdescr               = ls_search_help-intdescr
                it_fielddescr            = ls_search_help-fielddescr
                it_filter_select_options = lt_selopt
              IMPORTING
                ev_dbtab_syntax          = lv_dbtab_syntax
                ev_select                = lv_select
                et_where_clauses         = lt_where_clauses
              CHANGING
                rv_where_clause          = lv_where_clause.

          CATCH /iwbep/cx_mgw_tech_exception .
*            implement
        ENDTRY.

        CHECK lv_dbtab_syntax IS NOT INITIAL.

        LOOP AT lt_where_clauses INTO ls_where.
          IF sy-tabix > 1.
            APPEND 'AND' TO lt_where.
          ENDIF.
          APPEND LINES OF ls_where-where_tab TO lt_where.
        ENDLOOP.
        TRY.
            lv_tab = lv_dbtab_syntax.
            TRY.
                lv_dbtab_syntax = cl_abap_dyn_prg=>check_whitelist_str(
                     val       = lv_dbtab_syntax
                     whitelist = lv_tab
                        ).
              CATCH cx_abap_not_in_whitelist .
            ENDTRY.
            SELECT (lv_select) FROM (lv_dbtab_syntax)
                              INTO CORRESPONDING FIELDS OF TABLE @<values>
                              WHERE  (lt_where).
          CATCH /iwbep/cx_mgw_tech_exception.
        ENDTRY.
      ENDIF.

      IF lines( <values> ) = 0.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            textid           = /iwbep/cx_mgw_busi_exception=>business_error
            http_status_code = '404'
            message          = 'Value not found'(404).
      ELSE.
        er_entity = REF #( <values>[ 1 ] ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_entityset.
    DATA:
      lv_entity_type_name  TYPE /iwbep/mgw_tech_name,
      lv_domain            TYPE domname,
      lv_search_help       TYPE shlpname,
      lv_search_help_field TYPE fieldname,
      ls_search_help       TYPE shlp_descr_t,
      lv_raw_filter        TYPE string,
      ls_t100key           TYPE scx_t100key,
      lv_where_clause      TYPE string,
      lv_dbtab_syntax      TYPE string,
      lv_select            TYPE string,
      lt_where_clauses     TYPE STANDARD TABLE OF rsds_where,
      ls_where             TYPE rsds_where,
      lt_where             TYPE rsds_where_tab,
      lv_top               TYPE i,
      lv_skip              TYPE i,
      lv_where_clause2     TYPE string,
      lv_rows              TYPE i,
      lv_tab               TYPE string.

    FIELD-SYMBOLS:
      <values> TYPE STANDARD TABLE.

    lv_entity_type_name = io_tech_request_context->get_entity_type_name( ).
    lv_where_clause  = io_tech_request_context->get_osql_where_clause( ).
    lv_top = io_tech_request_context->get_top( ).
    IF lv_top > 0.
      lv_skip = io_tech_request_context->get_skip( ).
      " TODO: select one more to be sure below whether there is more
      lv_rows = lv_skip + lv_top. "Non-HANA only
    ELSE.
      lv_rows = 10000.
    ENDIF.
    IF lv_entity_type_name(6) = /clin/hcm_anno_shlp_annotation=>gc_fv.
      ev_done = abap_true.

      lv_domain = ddic_name( lv_entity_type_name+6 ).

      CREATE DATA er_entityset TYPE STANDARD TABLE OF dd07v.
      ASSIGN er_entityset->* TO <values>.

      /clin/hcm_anno_shlp_processor=>get_domain_values(
        EXPORTING
          iv_domain               = lv_domain
          io_tech_request_context = io_tech_request_context
        IMPORTING
          et_entityset            = <values>
          es_response_context     = es_response_context ).

    ELSEIF lv_entity_type_name(6) = /clin/hcm_anno_shlp_annotation=>gc_sh.
      ev_done = abap_true.

      lv_search_help       = ddic_name( lv_entity_type_name+6 ).
      lv_search_help_field = /clin/hcm_anno_shlp_processor=>get_focus( ir_request_details ).
      TRY.
          lv_raw_filter = ir_request_details->parameters[ name = '$filter' ]-value.
        CATCH cx_root.
      ENDTRY.

      /clin/hcm_anno_shlp_processor=>get_values_no_type_ahead(
        EXPORTING
          iv_search_help          = lv_search_help
          iv_search_help_field    = lv_search_help_field
          iv_raw_filter           = lv_raw_filter
          io_tech_request_context = io_tech_request_context
        IMPORTING
          er_entityset            = er_entityset
          es_response_context     = es_response_context ).

    ELSEIF lv_entity_type_name(6) = /clin/hcm_anno_shlp_annotation=>gc_ch
        OR lv_entity_type_name(6) = /clin/hcm_anno_shlp_annotation=>gc_ct.
      ev_done = abap_true.

      IF  io_tech_request_context->get_search_string( ) IS NOT INITIAL.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
          EXPORTING
            textid           = /iwbep/cx_mgw_tech_exception=>operation_not_supported
            http_status_code = /iwbep/cx_mgw_tech_exception=>gcs_http_status_codes-not_implemented
            operation        = 'search' "#EC NOTEXT
            entity_type      = io_tech_request_context->get_entity_type_name( ).
      ENDIF.

      DATA(lo_filter) = io_tech_request_context->get_filter( ).
      DATA(lt_selopt) = lo_filter->get_filter_select_options( ).
      DATA(lv_filter_string) = lo_filter->get_filter_string( ).

*      ls_search_help-shlpname = lv_entity_type_name+6. "INTEGRTR
      ls_search_help-shlpname       = ddic_name( lv_entity_type_name+6 ). "INTEGRTR
*      When shlptype is 'CT', Function Module DD_SHLP_GET_HELPMETHOD must be always called with type 'CH' (internal message 748242 2014)
*      ls_search_help-shlptype = lv_entity_type_name+3(2).
      ls_search_help-shlptype = /clin/hcm_anno_shlp_annotation=>gc_ch+3(2).

      CALL FUNCTION 'DD_SHLP_GET_HELPMETHOD'
        CHANGING
          shlp = ls_search_help.

      er_entityset = create_dynamic_table( it_fields = ls_search_help-fielddescr ).
      ASSIGN er_entityset->* TO <values>.



      IF lv_entity_type_name(6) = /clin/hcm_anno_shlp_annotation=>gc_ch.
        TRY.
            lv_tab = ls_search_help-intdescr-selmethod.
            TRY.
                ls_search_help-intdescr-selmethod = cl_abap_dyn_prg=>check_whitelist_str(
                     val       = ls_search_help-intdescr-selmethod
                     whitelist = lv_tab
                        ).
              CATCH cx_abap_not_in_whitelist .
            ENDTRY.
            REPLACE ALL OCCURRENCES OF '´' IN lv_where_clause WITH '@*>'.
            lv_where_clause = cl_abap_dyn_prg=>escape_quotes_str( val = lv_where_clause ).
            REPLACE ALL OCCURRENCES OF '@*>' IN lv_where_clause WITH '´'.
            SELECT * FROM (ls_search_help-intdescr-selmethod) INTO CORRESPONDING FIELDS OF TABLE <values>
             WHERE (lv_where_clause).
          CATCH /iwbep/cx_mgw_tech_exception.
        ENDTRY.
      ELSE.
        TRY.
            CALL METHOD /clin/hcm_anno_shlp_processor=>get_select_clause_ct
              EXPORTING
                i_checktab               = ls_search_help-intdescr-selmethod
                i_intdescr               = ls_search_help-intdescr
                it_fielddescr            = ls_search_help-fielddescr
                it_filter_select_options = lt_selopt
              IMPORTING
                ev_dbtab_syntax          = lv_dbtab_syntax
                ev_select                = lv_select
                et_where_clauses         = lt_where_clauses
              CHANGING
                rv_where_clause          = lv_where_clause.

          CATCH /iwbep/cx_mgw_tech_exception.
        ENDTRY.

        CHECK lv_dbtab_syntax IS NOT INITIAL.
        " TODO: if $select is specified, reduce to distinct values in the selected column combination

        LOOP AT lt_where_clauses INTO ls_where.
          IF sy-tabix > 1.
            APPEND 'AND' TO lt_where.
          ENDIF.
          APPEND LINES OF ls_where-where_tab TO lt_where.
        ENDLOOP.
        TRY.
            lv_tab = lv_dbtab_syntax.
            TRY.
                lv_dbtab_syntax = cl_abap_dyn_prg=>check_whitelist_str(
                     val       = lv_dbtab_syntax
                     whitelist = lv_tab
                        ).
              CATCH cx_abap_not_in_whitelist .
            ENDTRY.

            SELECT (lv_select) UP TO @lv_rows ROWS
                              FROM (lv_dbtab_syntax)
                              INTO CORRESPONDING FIELDS OF TABLE @<values>
                              WHERE  (lt_where).
            IF lv_skip > 0.
              DELETE  <values> TO lv_skip.              "#EC CI_NOORDER
            ENDIF.
          CATCH /iwbep/cx_mgw_tech_exception.
        ENDTRY.
      ENDIF.

      SORT <values>.

    ENDIF.

  ENDMETHOD.


  METHOD get_focus.
    TRY.
        DATA(lv_uri) = ir_request_details->technical_request-request_header[ name = '~request_uri' ]-value.
        DATA(lv_options) = segment( val = lv_uri index = 2 sep = '?' ).
        DATA(lt_fields) = cl_http_utility=>string_to_fields( lv_options ).

        rv_focus = lt_fields[ name = 'search-focus' ]-value. "#EC NOTEXT
      CATCH cx_root.                                    "#EC NO_HANDLER
    ENDTRY.
  ENDMETHOD.


  METHOD get_select_clause_ct.
    DATA: ls_shlpfld        TYPE dfies,
          lv_string1        TYPE string,
          lv_string2        TYPE string,
          lv_select         TYPE string,
          lv_langu          TYPE string,
          lt_x031l_2        TYPE  STANDARD TABLE OF x031l,
          ls_x031l          TYPE x031l,
          lt_dfies          TYPE STANDARD TABLE OF dfies,
          ls_dfies          TYPE dfies,
          lv_add_tilde      TYPE string,
          lv_add_comma      TYPE string,
          lt_field_ranges   TYPE rsds_trange,
          lt_field_ranges_a TYPE rsds_trange,
          lt_field_ranges_b TYPE rsds_trange,
          ls_where          TYPE rsds_where,
          lt_where          TYPE rsds_twhere.

    CLEAR: ev_select, ev_dbtab_syntax.

*-- No Text Table
    IF i_intdescr-texttab IS INITIAL.
      LOOP AT it_fielddescr INTO ls_shlpfld.
        IF ev_select IS INITIAL.
          lv_add_comma = ||.
        ELSE.
          lv_add_comma = |,|.
        ENDIF.
        IF ls_shlpfld-tabname = i_intdescr-selmethod.
          ev_select = ev_select && lv_add_comma && ls_shlpfld-fieldname.
        ENDIF.
        LOOP AT it_filter_select_options INTO DATA(ls_filter_select_option_wttext).
*- add to range
          IF ls_filter_select_option_wttext-property = ls_shlpfld-fieldname.
            LOOP AT ls_filter_select_option_wttext-select_options INTO DATA(select_option).
              REFRESH lt_field_ranges_a.
              CALL METHOD /clin/hcm_anno_shlp_processor=>build_where_clause_from_filter
                EXPORTING
                  iv_tabname              = ls_shlpfld-tabname
                  iv_attribute_name       = ls_shlpfld-fieldname
                  is_filter_select_option = select_option
                IMPORTING
                  et_field_ranges         = lt_field_ranges_a.
              IF lt_field_ranges_a IS NOT INITIAL.
                APPEND LINES OF lt_field_ranges_a TO lt_field_ranges.
              ENDIF.
            ENDLOOP.
            CONTINUE.
          ENDIF.
        ENDLOOP.
        ev_dbtab_syntax = |( | && i_intdescr-selmethod &&  | )|.
      ENDLOOP.

      CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
        EXPORTING
          field_ranges  = lt_field_ranges_a
        IMPORTING
          where_clauses = et_where_clauses.
      EXIT.
    ENDIF.

*-- Text Table
    lv_string1 = 'on'.                                      "#EC NOTEXT
    LOOP AT it_fielddescr INTO ls_shlpfld.
      IF ls_shlpfld-tabname = i_intdescr-selmethod.
        IF lv_select IS INITIAL.
          lv_add_tilde = | a~|.                             "#EC NOTEXT
        ELSE.
          lv_add_tilde = |, a~|.                            "#EC NOTEXT
        ENDIF.
        lv_select = lv_select && lv_add_tilde && ls_shlpfld-fieldname.
        LOOP AT it_filter_select_options INTO DATA(ls_filter_select_option).
*- add to range
          IF ls_filter_select_option-property = ls_shlpfld-fieldname.
            LOOP AT ls_filter_select_option-select_options INTO DATA(select_option_a).
              REFRESH lt_field_ranges_a.
              CALL METHOD /clin/hcm_anno_shlp_processor=>build_where_clause_from_filter
                EXPORTING
                  iv_tabname              = ls_shlpfld-tabname
                  iv_attribute_name       = |a~| && ls_shlpfld-fieldname "#EC NOTEXT
                  is_filter_select_option = select_option_a
                IMPORTING
                  et_field_ranges         = lt_field_ranges_a.
              IF lt_field_ranges_a IS NOT INITIAL.
                APPEND LINES OF lt_field_ranges_a TO lt_field_ranges.
              ENDIF.
            ENDLOOP.
            CONTINUE.
          ENDIF.
        ENDLOOP.
      ELSE.
        IF lv_select IS INITIAL.
          lv_add_tilde = | b~|.                             "#EC NOTEXT
        ELSE.
          lv_add_tilde = |, b~|.                            "#EC NOTEXT
        ENDIF.
        lv_select = lv_select && lv_add_tilde && ls_shlpfld-fieldname.
*-range
        LOOP AT it_filter_select_options INTO DATA(ls_filter_select_option_b).
          IF ls_filter_select_option_b-property = ls_shlpfld-fieldname.
            LOOP AT ls_filter_select_option_b-select_options INTO DATA(select_option_b).
              REFRESH lt_field_ranges_b.
              CALL METHOD /clin/hcm_anno_shlp_processor=>build_where_clause_from_filter
                EXPORTING
                  iv_tabname              = ls_shlpfld-tabname
                  iv_attribute_name       = |b~| && ls_shlpfld-fieldname "#EC NOTEXT
                  is_filter_select_option = select_option_b
                IMPORTING
                  et_field_ranges         = lt_field_ranges_b.
              IF lt_field_ranges_b IS NOT INITIAL.
                APPEND LINES OF lt_field_ranges_b TO lt_field_ranges.
              ENDIF.
            ENDLOOP.
            CONTINUE.
          ENDIF.
        ENDLOOP.
      ENDIF.

      CHECK ls_shlpfld-keyflag IS NOT INITIAL.
      IF sy-tabix <> 1.
        lv_string1 = lv_string1 && ' and '.                 "#EC NOTEXT
        lv_string2 = lv_string2 && ' and '.                 "#EC NOTEXT
      ENDIF.
      lv_string1 = lv_string1 &&  ' a~' && ls_shlpfld-fieldname && ' = ' && ' b~' && ls_shlpfld-fieldname. "#EC NOTEXT
      lv_string2 = lv_string2 &&  ' a~' && ls_shlpfld-fieldname && ' = ' && ' b~' && ls_shlpfld-fieldname. "#EC NOTEXT
    ENDLOOP.


    CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
      EXPORTING
        field_ranges  = lt_field_ranges
      IMPORTING
        where_clauses = et_where_clauses.

*--
    ev_select = lv_select.
    ev_dbtab_syntax = `( # AS a `
        & ` LEFT JOIN % AS b `  && lv_string1.              "#EC NOTEXT

    REPLACE '#' WITH i_intdescr-selmethod INTO ev_dbtab_syntax.
    REPLACE '%' WITH i_intdescr-texttab   INTO ev_dbtab_syntax.

*get the fields of i_intdescr-texttab
    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        tabname   = i_intdescr-texttab
      TABLES
        x031l_tab = lt_x031l_2
        dfies_tab = lt_dfies.

    LOOP AT lt_x031l_2 INTO ls_x031l.
      CHECK ( ls_x031l-fieldname = 'SPRAS' OR ls_x031l-fieldname = 'LANGU'). "#EC NOTEXT
      READ TABLE lt_dfies INTO ls_dfies WITH KEY fieldname = ls_x031l-fieldname.
      CHECK sy-subrc = 0 AND ls_dfies-keyflag = 'X'.

      lv_langu = ' and b~' && ls_x031l-fieldname && | = @SY-LANGU |. "#EC NOTEXT

      EXIT.
    ENDLOOP.

    ev_dbtab_syntax  = ev_dbtab_syntax && | | && lv_langu && |)|.

    CHECK  rv_where_clause IS NOT INITIAL.
*      CALL METHOD CL_O2C_FICA_SHLP_PROCESSOR=>add_table_2_where_string
*        EXPORTING
*          iv_checktable    = i_intdescr-selmethod
*          iv_texttable     = i_intdescr-texttab
*        CHANGING
*          cv_filter_string = rv_where_clause.
*    rv_where_clause = |( |  && rv_where_clause  && | )|.

  ENDMETHOD.


  METHOD get_values.

    /clin/hcm_anno_shlp_processor=>get_values_no_type_ahead(
      EXPORTING
        iv_search_help          = iv_search_help
        iv_search_help_field    = iv_search_help_field
        io_tech_request_context = io_tech_request_context
      IMPORTING
        et_entityset            = et_entityset
        es_response_context     = es_response_context ).

  ENDMETHOD.


  METHOD get_values_no_type_ahead.
    DATA:
      lt_filter   TYPE /iwbep/t_mgw_select_option,
      lt_sort     TYPE abap_sortorder_tab,
      ls_sort     LIKE LINE OF lt_sort,
      lo_tap      TYPE REF TO if_dsh_type_ahead_processor,
      ls_param    TYPE if_dsh_type_ahead_processor=>ty_s_additional_parameters,
      ls_field    LIKE LINE OF ls_param-field_values,
      lv_req      TYPE if_dsh_type_ahead_processor=>ty_type_ahead_request,
      lo_type     TYPE REF TO cl_abap_tabledescr,
      lv_extnd_so TYPE abap_bool VALUE abap_false,
      lv_top      TYPE i,
      lv_skip     TYPE i,
      lv_rows     TYPE i,
      ld_values   TYPE REF TO data,
      lt_keys     TYPE /iwbep/t_mgw_tech_pairs,
      lo_cx       TYPE REF TO cx_root.

    FIELD-SYMBOLS:
      <values> TYPE STANDARD TABLE.

*      if iv_search_help_field is initial.
*        exit.
*      endif.
    SET PARAMETER ID 'SDSH_FUZZY_SEARCH' FIELD abap_true .
*-- EntitySet
    IF io_tech_request_context IS BOUND.
      lt_filter = io_tech_request_context->get_filter( )->get_filter_select_options( ).
      DATA(lv_filter_string) = io_tech_request_context->get_filter( )->get_filter_string( ).
      IF lt_filter IS INITIAL AND lv_filter_string IS NOT INITIAL.
        IF iv_raw_filter IS NOT SUPPLIED.
          CREATE OBJECT lo_cx TYPE cx_fkk_error
            EXPORTING
              textid        = cx_fkk_error=>invalid_filter
              filter_string = lv_filter_string.
        ELSE.
          " try converting with SAIL converter
          TRY.
              lt_filter = cl_clb2_tools=>odata_filter2select_option( iv_raw_filter ).
            CATCH cx_clb2_parse INTO lo_cx.
          ENDTRY.
        ENDIF.
      ENDIF.
      IF lo_cx IS NOT INITIAL.
        " both select-option converters failed
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
          EXPORTING
            previous         = lo_cx
            textid           = /iwbep/cx_mgw_tech_exception=>deserializ_error
            http_status_code = /iwbep/cx_mgw_tech_exception=>gcs_http_status_codes-not_implemented
            operation        = '$filter' "#EC NOTEXT
            entity_type      = io_tech_request_context->get_entity_type_name( ).
      ENDIF.

      LOOP AT lt_filter REFERENCE INTO DATA(ld_filter).
        CLEAR ls_field.
        ls_field-name = ld_filter->property.
        IF ls_field-name CO iv_search_help_field." or iv_search_help_field is initial.
          lv_extnd_so = abap_true.
        ENDIF.
        MOVE-CORRESPONDING ld_filter->select_options TO ls_field-params.
        INSERT ls_field INTO TABLE ls_param-field_values.
      ENDLOOP.

      IF lt_filter IS NOT INITIAL.
        lv_extnd_so = abap_true.
      ENDIF.

      DATA(lt_order) = io_tech_request_context->get_orderby( ).
      LOOP AT lt_order REFERENCE INTO DATA(ld_order).
        CLEAR ls_sort.
        ls_sort-name = ld_order->property.
        IF ld_order->order = 'desc'.
          ls_sort-descending = abap_true.
        ENDIF.
        APPEND ls_sort TO lt_sort.
      ENDLOOP.
    ENDIF.

    "Begin: Conversion of DATS from internal format to external format to meet F4's expectation, i039500, 2014.10.28
    DATA lo_request     TYPE REF TO /iwbep/cl_mgw_request.
    DATA lv_dat         TYPE d.
    FIELD-SYMBOLS:
      <ls_param>        TYPE rsdsselopt,
      <ls_field_values> TYPE if_dsh_type_ahead_processor=>ty_s_field_value.
    TRY.
        lo_request ?= io_tech_request_context.
        IF lo_request IS NOT INITIAL.
          DATA(ls_req_details) = lo_request->get_request_details( ).

          LOOP AT ls_param-field_values ASSIGNING <ls_field_values>.
            READ TABLE ls_req_details-technical_request-filter_expressions WITH KEY l_oprnd_int = <ls_field_values>-name literal_type = 'DateTime' TRANSPORTING NO FIELDS.
            IF sy-subrc = 0.
              LOOP AT <ls_field_values>-params ASSIGNING <ls_param>.
                IF <ls_param>-high IS NOT INITIAL.
                  lv_dat = <ls_param>-high.
                  cl_abap_datfm=>conv_date_int_to_ext( EXPORTING im_datint = lv_dat IMPORTING ex_datext = <ls_param>-high ).
                ENDIF.
                IF <ls_param>-low IS NOT INITIAL.
                  lv_dat = <ls_param>-low.
                  cl_abap_datfm=>conv_date_int_to_ext( EXPORTING im_datint = lv_dat IMPORTING ex_datext = <ls_param>-low ).
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDLOOP.
        ENDIF.
      CATCH cx_sy_assign_cast_illegal_cast cx_abap_datfm_format_unknown INTO DATA(ex_cast).
        "Suppose no DateTime parameters to be handled.
    ENDTRY.
    "End

*-- Entity
    IF io_tech_request_cntxt IS BOUND.
      lt_keys = io_tech_request_cntxt->get_keys( ).
      LOOP AT lt_keys INTO DATA(ls_key).
        ls_key-value = cl_abap_dyn_prg=>escape_quotes( ls_key-value ).
        CLEAR ls_field.
        ls_field-name = ls_key-name.
        ls_field-params = VALUE #( ( low = ls_key-value  option = 'EQ'  sign = 'I' ) ) .
        INSERT ls_field INTO TABLE ls_param-field_values.
      ENDLOOP.
    ENDIF.

    TRY.
        "Begin: To replace cl_fis_dh_type_ahead_processor by cl_dsh_type_ahead_processor, i039500, 2014.10.17
        lo_tap = cl_dsh_type_ahead_processor=>create_instance_for_shlp(
         i_search_help_name             = iv_search_help
         i_referenced_field_name        = iv_search_help_field
         i_additional_parameters        = ls_param
         i_use_extended_select_options  = lv_extnd_so
         i_force_type_ahead             = lv_extnd_so
        ).
*           lo_tap = cl_fis_dh_type_ahead_processor=>create_instance_for_shlp(
*                   i_search_help_name      = iv_search_help
*                   i_referenced_field_name = iv_search_help_field
*                   i_additional_parameters = ls_param
*                  ).
        "End 2014.10.17
*       check  lo_tap->is_type_ahead_supported( ) = abap_true.
        lo_type = lo_tap->get_result_descriptor( ).
        CREATE DATA ld_values TYPE HANDLE lo_type.
        ASSIGN ld_values->* TO <values>.

        IF io_tech_request_context IS BOUND.
          lv_req = io_tech_request_context->get_search_string( ).

          lv_top = io_tech_request_context->get_top( ).
          IF lv_top > 0.
            lv_skip = io_tech_request_context->get_skip( ).
            " TODO: select one more to be sure below whether there is more
            lv_rows = lv_skip + lv_top.
          ELSE.
            lv_rows = 10000.
          ENDIF.
        ENDIF.

        lo_tap->get_type_ahead_values(
          EXPORTING i_type_ahead_request = lv_req
                    i_max_results        = lv_rows
          IMPORTING e_type_ahead_values  = <values> ).

        IF lv_skip > 0.
          DELETE <values> TO lv_skip.                   "#EC CI_NOORDER
        ENDIF.

        IF lt_filter[] IS NOT INITIAL.
          IF lines( lt_sort ) <> 0.
*          sort <values>.
*        else.
            SORT <values> BY (lt_sort).
          ENDIF.
        ENDIF.

*        es_response_context-inlinecount = lines( <values> ).
*        if es_response_context-inlinecount = lv_rows.
*          " assume there is at least one more matching entry beyond lv_rows
*          add 1 to es_response_context-inlinecount.
*        endif.

*        if lv_skip > 0.
*          delete <values> to lv_skip.                   "#EC CI_NOORDER
*        endif.

        IF er_entityset IS SUPPLIED.
          er_entityset = ld_values.
        ENDIF.

        IF er_entity IS SUPPLIED.
          er_entity = REF #( <values>[ 1 ] ).
        ENDIF.

        IF et_entityset IS SUPPLIED.
          MOVE-CORRESPONDING <values> TO et_entityset.
        ENDIF.
      CATCH cx_root INTO lo_cx.                          "#EC CATCH_ALL
        "TODO: add link to note for this case, consider using special exception
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
          EXPORTING
            previous         = lo_cx
            textid           = /iwbep/cx_mgw_tech_exception=>operation_not_supported
            http_status_code = /iwbep/cx_mgw_tech_exception=>gcs_http_status_codes-not_implemented
            operation        = 'search' "#EC NOTEXT
            entity_type      = io_tech_request_context->get_entity_type_name( ).
    ENDTRY.

  ENDMETHOD.


  METHOD raise_technical_exception.
    IF iv_add_leading_msg = abap_true.
      go_msg_container->add_message(
          iv_msg_type           = iv_msg_type
          iv_msg_id             = iv_msg_id
          iv_msg_number         = iv_msg_number
          iv_msg_v1             = iv_msg_v1
          iv_msg_v2             = iv_msg_v2
          iv_msg_v3             = iv_msg_v3
          iv_msg_v4             = iv_msg_v4
          iv_is_leading_message = abap_true ).
    ENDIF.

    RAISE EXCEPTION TYPE /iwbep/cx_mgw_tech_exception
      EXPORTING
        previous          = ix_previous
        message_container = go_msg_container
        http_status_code  = iv_http_status_code.
  ENDMETHOD.
ENDCLASS.
