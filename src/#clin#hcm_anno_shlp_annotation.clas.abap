class /CLIN/HCM_ANNO_SHLP_ANNOTATION definition
  public
  final
  create public .

public section.

  types:
    begin of TS_SEARCH_HELP,
      shlpname type shlpname,
      shlptype type ddshlptyp,
    end of ts_search_help .
  types:
    ty_tt_tabname_fieldname type standard table of ddtbfd with default key .
  types:
    tt_search_helps type standard table of shlpname with default key .

  constants GC_CH type CHAR6 value 'VL_CH_' ##NO_TEXT.
  constants GC_CT type CHAR6 value 'VL_CT_' ##NO_TEXT.
  constants GC_FV type CHAR6 value 'VL_FV_' ##NO_TEXT.
  constants GC_SH type CHAR6 value 'VL_SH_' ##NO_TEXT.

  class-methods ENTITY_TYPE_NAME
    importing
      !IV_SHLPNAME type SHLPNAME
      !IV_SHLPTYPE type DDSHLPTYP default 'SH'
    returning
      value(RV_ODATA_NAME) type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME .
  class-methods FIXED_VALUES
    importing
      !IO_ODATA_MODEL type ref to /IWBEP/IF_MGW_ODATA_MODEL
      !IV_SHLPNAME type SHLPNAME
      !IV_SHLPTYPE type DDSHLPTYP default 'SH'
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  class-methods SEARCH_HELP
    importing
      !IV_ENTITYSET_NAME type CSEQUENCE
    returning
      value(RS_SEARCH_HELP) type TS_SEARCH_HELP .
  class-methods PROPERTY_NAME
    importing
      !IV_FIELDNAME type CSEQUENCE
    returning
      value(RV_PROPERTY_NAME) type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME .
  methods ADD_INOUT_PARAMETER
    importing
      !IV_PROPERTY type STRING
      !IV_VALUELIST_PROPERTY type STRING
    returning
      value(RO_ANNOTATION) type ref to /CLIN/HCM_ANNO_SHLP_ANNOTATION .
  methods ADD_IN_PARAMETER
    importing
      !IV_PROPERTY type STRING
      !IV_VALUELIST_PROPERTY type STRING
    returning
      value(RO_ANNOTATION) type ref to /CLIN/HCM_ANNO_SHLP_ANNOTATION .
  methods ADD_DISPLAY_PARAMETER
    importing
      !IV_VALUELIST_PROPERTY type STRING
    returning
      value(RO_ANNOTATION) type ref to /CLIN/HCM_ANNO_SHLP_ANNOTATION .
  methods ADD_OUT_PARAMETER
    importing
      !IV_PROPERTY type STRING
      !IV_VALUELIST_PROPERTY type STRING
    returning
      value(RO_ANNOTATION) type ref to /CLIN/HCM_ANNO_SHLP_ANNOTATION .
  class-methods CREATE
    importing
      !IO_ODATA_MODEL type ref to /IWBEP/IF_MGW_ODATA_MODEL
      !IO_VOCAN_MODEL type ref to /IWBEP/IF_MGW_VOCAN_MODEL
      !IV_NAMESPACE type STRING
      !IV_ENTITYTYPE type STRING
      !IV_PROPERTY type STRING
      !IV_SEARCH_HELP type SHLPNAME optional
      !IV_SEARCH_SUPPORTED type ABAP_BOOL optional
      !IV_SEARCH_HELP_FIELD type FIELDNAME optional
      !IV_QUALIFIER type STRING optional
      !IV_LABEL type CSEQUENCE optional
      !IV_VALUELIST_ENTITYSET type STRING
      !IV_VALUELIST_PROPERTY type STRING
      !IV_VALUELIST_SERVICE_NAME type /IWBEP/MED_GRP_TECHNICAL_NAME optional
      !IV_VALUELIST_SERVICE_VERSION type /IWBEP/MED_GRP_VERSION default 1
    returning
      value(RO_ANNOTATION) type ref to /CLIN/HCM_ANNO_SHLP_ANNOTATION
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION
      CX_FKK_ERROR .
  class-methods CREATE_FOR_ALL
    importing
      !IO_ODATA_MODEL type ref to /IWBEP/IF_MGW_ODATA_MODEL
      !IO_VOCAN_MODEL type ref to /IWBEP/IF_MGW_VOCAN_MODEL
      !IV_NAMESPACE type STRING
      !IT_FIELDS type TY_TT_TABNAME_FIELDNAME optional
      !IT_EXCLUDED_FIELDS type TY_TT_TABNAME_FIELDNAME optional
      !IT_EXCLUDED_SEARCH_HELPS type TT_SEARCH_HELPS optional
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  protected section.
private section.

  data MO_PARAMETERS type ref to /IWBEP/IF_MGW_VOCAN_COLLECTION .
  class-data SV_NAMESPACE type STRING .
  class-data SO_TEXT_ANNOTATION type ref to /CLIN/HCM_ANNO_TEXT_ANNOTATION .

  class-methods SEARCH_HELP_IS_USABLE
    importing
      !IS_SEARCH_HELP type SHLP_DESCR
    returning
      value(RV_IS_USABLE) type ABAP_BOOL .
  class-methods ADD_ENTITY_TYPE
    importing
      !IV_ANNOTATED_ENTITY_TYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME
      value(IS_SEARCH_HELP) type SHLP_DESCR
      !IO_ODATA_MODEL type ref to /IWBEP/IF_MGW_ODATA_MODEL
    exporting
      !EV_VALUELIST_ENTITYSET type STRING
    changing
      !CT_TYPES type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_T_MED_ENTITY_TYPES
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  class-methods ADJUST_KEYS
    changing
      !CS_SEARCH_HELP type SHLP_DESCR .
  methods ADD_FILTERONLY_PARAMETER
    importing
      !IV_VALUELIST_PROPERTY type STRING
    returning
      value(RO_ANNOTATION) type ref to /CLIN/HCM_ANNO_SHLP_ANNOTATION .
  class-methods ENTITY_SET_NAME
    importing
      !IS_SEARCH_HELP type SHLP_DESCR
    returning
      value(RV_ODATA_NAME) type STRING .
  class-methods DDIC_NAME
    importing
      !IV_ODATA_NAME type CSEQUENCE
    returning
      value(RV_DDIC_NAME) type SHLPNAME .
  class-methods ODATA_NAME
    importing
      !IV_DDIC_NAME type CSEQUENCE
    returning
      value(RV_ODATA_NAME) type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME .
  class-methods CREATE_ONE
    importing
      !IO_ODATA_MODEL type ref to /IWBEP/IF_MGW_ODATA_MODEL
      !IO_VOCAN_MODEL type ref to /IWBEP/IF_MGW_VOCAN_MODEL
      !IV_NAMESPACE type STRING
      !IV_ENTITYTYPE type STRING
      !IV_PROPERTY type STRING
      !IV_QUALIFIER type STRING optional
      !IV_LABEL type CSEQUENCE optional
      !IV_VALUELIST_ENTITYSET type STRING
      !IV_VALUELIST_SERVICE_NAME type /IWBEP/MED_GRP_TECHNICAL_NAME optional
      !IV_VALUELIST_SERVICE_VERSION type /IWBEP/MED_GRP_VERSION default 1
      !IV_SEARCH_SUPPORTED type ABAP_BOOL
    returning
      value(RO_ANNOTATION) type ref to /CLIN/HCM_ANNO_SHLP_ANNOTATION .
  class-methods GET_SEARCH_HELPS
    importing
      !IV_FIELDNAME type FIELDNAME
      !IV_STRUCTNAME type TABNAME
    returning
      value(RT_SEARCH_HELP) type SHLP_DESCT .
ENDCLASS.



CLASS /CLIN/HCM_ANNO_SHLP_ANNOTATION IMPLEMENTATION.


  method ADD_DISPLAY_PARAMETER.
    data:
      lo_record type ref to /iwbep/if_mgw_vocan_record.

    lo_record = mo_parameters->create_record( 'com.sap.vocabularies.Common.v1.ValueListParameterDisplayOnly' ).
    lo_record->create_property( 'ValueListProperty' )->create_simple_value( )->set_string( iv_valuelist_property ).

    ro_annotation = me.
  endmethod.


  METHOD ADD_ENTITY_TYPE.
    DATA:
      ls_type        LIKE LINE OF ct_types,
      lo_entity_type TYPE REF TO /iwbep/if_mgw_odata_entity_typ,
      lo_entity_set  TYPE REF TO /iwbep/if_mgw_odata_entity_set,
      lo_property    TYPE REF TO /iwbep/if_mgw_odata_property.

    CHECK search_help_is_usable( is_search_help ) = abap_true.

    ev_valuelist_entityset = entity_set_name( is_search_help ).

    ls_type-name = ev_valuelist_entityset.
    READ TABLE ct_types TRANSPORTING NO FIELDS WITH KEY name = ls_type-name.
    IF sy-subrc = 0.
      " been here, done this
      ev_valuelist_entityset = ls_type-name.
      RETURN.
    ELSEIF iv_annotated_entity_type(6) = gc_ch
        OR iv_annotated_entity_type(6) = gc_ct
        OR iv_annotated_entity_type(6) = gc_sh.
      " don't add value list entity sets for our artificial types
      CLEAR ev_valuelist_entityset.
      RETURN.
    ENDIF.

    ls_type-external_name = ls_type-name.
    ls_type-attribute_struct = is_search_help-intdescr-selmethod.

    lo_entity_type = io_odata_model->create_entity_type( ls_type-external_name ).

    lo_entity_set = lo_entity_type->create_entity_set( ls_type-external_name ).
    lo_entity_set->set_creatable( abap_false ).
    lo_entity_set->set_updatable( abap_false ).
    lo_entity_set->set_deletable( abap_false ).
    lo_entity_set->create_annotation( /iwbep/if_mgw_med_odata_types=>gc_sap_namespace
    )->add(
      iv_key   = 'countable'  ##NO_TEXT
      iv_value = 'false'      ##NO_TEXT
    ).
    IF is_search_help-shlptype = 'CH' OR is_search_help-shlptype = 'CT'.
      " create entity set annotated as "aggregate", i.e. $select will trigger SELECT DISTINCT
      lo_entity_set->create_annotation( /iwbep/if_mgw_med_odata_types=>gc_sap_namespace
      )->add(
        iv_key   = 'semantics'  ##NO_TEXT
        iv_value = /iwbep/if_ana_odata_types=>gcs_ana_odata_semantic_value-query-aggregate && ``
      ).
    ENDIF.

    " TODO: try to detect which CHAR field is the text field for the last key field
    " TODO: then add a sap:text annotation to the last key field pointing to the text field
    " TODO: should be easy for check tables with text table (CT): the text field is the only field from the text table

    adjust_keys( CHANGING cs_search_help = is_search_help ).

    SORT is_search_help-fieldprop BY shlpselpos.

    LOOP AT is_search_help-fieldprop REFERENCE INTO DATA(ld_fieldprop).
      READ TABLE is_search_help-fielddescr REFERENCE INTO DATA(ld_fielddescr) WITH KEY fieldname = ld_fieldprop->fieldname.

      lo_property = lo_entity_type->create_property(
        iv_property_name  = odata_name( ld_fielddescr->fieldname )
        iv_abap_fieldname = ld_fielddescr->fieldname ).

      lo_property->bind_data_element( ld_fielddescr->rollname && `` ).

      IF ld_fielddescr->convexit IS NOT INITIAL.
        lo_property->set_conversion_exit( ld_fielddescr->convexit ).
        lo_property->enable_conversion( ).
      ENDIF.

      IF ld_fielddescr->keyflag = abap_true.
        lo_property->set_is_key( ).
      ENDIF.

      IF ld_fieldprop->shlpinput = abap_false AND ld_fieldprop->shlpselpos = '00'.
        lo_property->set_filterable( abap_false ).
      ENDIF.

      IF ld_fielddescr->datatype = 'CUKY'.
        lo_property->set_semantic( 'currency-code' ).
        IF ld_fielddescr->keyflag = abap_true.
          " ISO codes in TCURC are not unique, so we have to use the internal SAP codes here
          lo_property->disable_conversion( ).
        ENDIF.
      ENDIF.

      " set sap:label based on mapped data element
      lo_property->set_text_key(
        iv_text_key = so_text_annotation->get_mapped_dtel( ld_fielddescr->rollname )
        iv_text_object_type = 'B'
        iv_create = abap_true ).

      CASE ld_fielddescr->inttype.
        WHEN cl_abap_typedescr=>typekind_string.
          lo_property->set_type_edm_string( ).
          lo_property->set_internal_type( ld_fielddescr->inttype ).
        WHEN cl_abap_typedescr=>typekind_char.
          " TODO: add more domains that are used as Boolean with 'X' = true, ' ' = false
          IF ld_fielddescr->domname = 'AS4FLAG'
          OR ld_fielddescr->domname = 'XFELD'
          OR ( ld_fielddescr->domname CP '*BOOL*' AND ld_fielddescr->leng = 1 ).
            lo_property->set_type_edm_boolean( ).
          ELSE.
            lo_property->set_type_edm_string( ).
            IF ld_fielddescr->lowercase = abap_false.
              lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation(
                iv_annotation_namespace =  /iwbep/if_mgw_med_odata_types=>gc_sap_namespace
              )->add(
                iv_key   = /iwbep/if_ana_odata_types=>gcs_ana_odata_annotation_key-display_format
                iv_value = 'UpperCase'          ##NO_TEXT
             ).
            ENDIF.
          ENDIF.
          lo_property->set_internal_type( ld_fielddescr->inttype ).
          lo_property->set_internal_length( ld_fielddescr->intlen + 0 ).
          lo_property->set_maxlength( ld_fielddescr->outputlen + 0 ).
        WHEN cl_abap_typedescr=>typekind_packed.
          lo_property->set_type_edm_decimal( ).
          lo_property->set_precison( ld_fielddescr->decimals + 0 ).
          lo_property->set_internal_type( ld_fielddescr->inttype ).
          lo_property->set_internal_length( ld_fielddescr->intlen + 0 ).
          lo_property->set_maxlength( 2 * ld_fielddescr->intlen - 1 ).
        WHEN cl_abap_typedescr=>typekind_date.
          lo_property->set_type_edm_datetime( ).
          lo_property->set_internal_type( ld_fielddescr->inttype ).
          lo_property->set_internal_length( ld_fielddescr->intlen + 0 ).
          lo_property->set_maxlength( ld_fielddescr->outputlen + 0 ).
          lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation(
            iv_annotation_namespace =  /iwbep/if_mgw_med_odata_types=>gc_sap_namespace
          )->add(
            iv_key   = /iwbep/if_ana_odata_types=>gcs_ana_odata_annotation_key-display_format
            iv_value = 'Date'          ##NO_TEXT
         ).
        WHEN cl_abap_typedescr=>typekind_num.
          lo_property->set_type_edm_string( ).
          lo_property->set_internal_type( ld_fielddescr->inttype ).
          lo_property->set_internal_length( ld_fielddescr->intlen + 0 ).
          lo_property->set_maxlength( ld_fielddescr->outputlen + 0 ).
          " TODO: new annotation for NUMC?
          lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation(
            iv_annotation_namespace =  /iwbep/if_mgw_med_odata_types=>gc_sap_namespace
          )->add(
            iv_key   = /iwbep/if_ana_odata_types=>gcs_ana_odata_annotation_key-display_format
            iv_value = 'NonNegative'
         ).
        WHEN cl_abap_typedescr=>typekind_int.
          lo_property->set_type_edm_int32( ).
          lo_property->set_internal_type( ld_fielddescr->inttype ).
        WHEN cl_abap_typedescr=>typekind_int2.
          lo_property->set_type_edm_int16( ).
          lo_property->set_internal_type( ld_fielddescr->inttype ).
        WHEN cl_abap_typedescr=>typekind_int1.
          lo_property->set_type_edm_byte( ).
          lo_property->set_internal_type( ld_fielddescr->inttype ).
        WHEN cl_abap_typedescr=>typekind_float
          OR cl_abap_typedescr=>typekind_decfloat16
          OR cl_abap_typedescr=>typekind_decfloat34.
          lo_property->set_type_edm_double( ).
          lo_property->set_internal_type( ld_fielddescr->inttype ).
        WHEN cl_abap_typedescr=>typekind_xstring.
          lo_property->set_type_edm_binary( ).
          lo_property->set_internal_type( ld_fielddescr->inttype ).
          lo_property->set_maxlength( ld_fielddescr->outputlen + 0 ).
        WHEN cl_abap_typedescr=>typekind_hex.
          lo_property->set_type_edm_binary( ).
          lo_property->set_internal_type( ld_fielddescr->inttype ).
          lo_property->set_internal_length( ld_fielddescr->intlen + 0 ).
          lo_property->set_maxlength( ld_fielddescr->outputlen + 0 ).
        WHEN cl_abap_typedescr=>typekind_time.
          lo_property->set_type_edm_time( ).
          lo_property->set_internal_type( ld_fielddescr->inttype ).
        WHEN OTHERS.
          lo_property->set_type_edm_string( ).
          lo_property->set_internal_type( cl_abap_typedescr=>typekind_string ).
      ENDCASE.
    ENDLOOP.

    DATA(lt_types) = CAST /iwbep/if_mgw_odata_re_model( io_odata_model )->get_entity_types( ).
    READ TABLE lt_types INTO ls_type WITH KEY name = ls_type-name.
    " Adding ValueList annotations to entity types created for search helps
    " approximately doubles the size of the metadata document, so we suppress the properties:
    CLEAR ls_type-properties.
    INSERT ls_type INTO TABLE ct_types.
  ENDMETHOD.                                             "#EC CI_VALPAR


  method ADD_FILTERONLY_PARAMETER.
    data:
      lo_record type ref to /iwbep/if_mgw_vocan_record.

    lo_record = mo_parameters->create_record( 'com.sap.vocabularies.Common.v1.ValueListParameterFilterOnly' ).
    lo_record->create_property( 'ValueListProperty' )->create_simple_value( )->set_string( iv_valuelist_property ).

    ro_annotation = me.
  endmethod.


  METHOD ADD_INOUT_PARAMETER.
    DATA:
      lo_record TYPE REF TO /iwbep/if_mgw_vocan_record.

    lo_record = mo_parameters->create_record( 'com.sap.vocabularies.Common.v1.ValueListParameterInOut' ).
    lo_record->create_property( 'LocalDataProperty' )->create_simple_value( )->set_property_path( iv_property ).
    lo_record->create_property( 'ValueListProperty' )->create_simple_value( )->set_string( iv_valuelist_property ).

*    mv_search_focus = iv_property.
*    lo_record->create_property( 'SearchFocus' )->create_simple_value( )->set_string( mv_search_focus ).


    ro_annotation = me.
  ENDMETHOD.


  method ADD_IN_PARAMETER.
    data:
      lo_record type ref to /iwbep/if_mgw_vocan_record.

    lo_record = mo_parameters->create_record( 'com.sap.vocabularies.Common.v1.ValueListParameterIn' ).
    lo_record->create_property( 'LocalDataProperty' )->create_simple_value( )->set_property_path( iv_property ).
    lo_record->create_property( 'ValueListProperty' )->create_simple_value( )->set_string( iv_valuelist_property ).

    ro_annotation = me.
  endmethod.


  method ADD_OUT_PARAMETER.
    data:
      lo_record type ref to /iwbep/if_mgw_vocan_record.

    lo_record = mo_parameters->create_record( 'com.sap.vocabularies.Common.v1.ValueListParameterOut' ).
    lo_record->create_property( 'LocalDataProperty' )->create_simple_value( )->set_property_path( iv_property ).
    lo_record->create_property( 'ValueListProperty' )->create_simple_value( )->set_string( iv_valuelist_property ).

    ro_annotation = me.
  endmethod.


  method ADJUST_KEYS.
    data:
      lt_fields     type standard table of dfies,
      ld_fielddescr type ref to dfies,
      lv_key_found  type abap_bool.

    case cs_search_help-intdescr-selmtype.
      when 'X'.
        return. " fielddescr->keyflag is correct for X
      when 'T'.
        " make sure all table key fields are marked as key

        call function 'DDIF_FIELDINFO_GET'
          exporting
            tabname   = cs_search_help-intdescr-selmethod
          tables
            dfies_tab = lt_fields
          exceptions
            others    = 0.

        loop at lt_fields reference into data(ld_field) where keyflag = abap_true.
          read table cs_search_help-fielddescr reference into ld_fielddescr with key fieldname = ld_field->fieldname.
          check sy-subrc = 0.
          ld_fielddescr->keyflag = abap_true.
          lv_key_found = abap_true.
        endloop.

      when 'V'.
        " the key flag is sometimes set for no field at all
        loop at cs_search_help-fielddescr reference into ld_fielddescr where keyflag = abap_true.
          lv_key_found = abap_true.
          exit.
        endloop.
    endcase.

    if lv_key_found = abap_false.
      " no key determined, so every property is key
      loop at cs_search_help-fielddescr reference into ld_fielddescr.
        ld_fielddescr->keyflag = abap_true.
      endloop.
    endif.

  endmethod.


method CREATE.
  data:
    ls_search_help      type shlp_descr_t,
    lv_search_supported type abap_bool,
    lv_label            type string.

  if iv_search_help is not initial.
    call function 'F4IF_GET_SHLP_DESCR'
      exporting
        shlpname = iv_search_help
      importing
        shlp     = ls_search_help.
    if ls_search_help-intdescr-issimple = abap_false.
      raise exception type cx_FKK_error
        exporting
          textid   = cx_FKK_error=>invalid_search_help
          shlpname = iv_search_help.
    endif.
    lv_search_supported = ls_search_help-intdescr-autosuggest.
  else.
    lv_search_supported = iv_search_supported.
  endif.

  if iv_label is supplied.
    lv_label = iv_label.
  else.
    lv_label = ls_search_help-intdescr-ddtext.
  endif.

  ro_annotation = create_one( io_odata_model               = io_odata_model
                              io_vocan_model               = io_vocan_model
                              iv_namespace                 = iv_namespace
                              iv_entitytype                = iv_entitytype
                              iv_property                  = iv_property
                              iv_qualifier                 = iv_qualifier
                              iv_valuelist_entityset       = iv_valuelist_entityset
                              iv_valuelist_service_name    = iv_valuelist_service_name
                              iv_valuelist_service_version = iv_valuelist_service_version
                              iv_label                     = lv_label
                              iv_search_supported          = lv_search_supported ).

  ro_annotation->add_inout_parameter(
    iv_property           = iv_property
    iv_valuelist_property = iv_valuelist_property
  ).

endmethod.


METHOD create_for_all.
  DATA:
    lv_structname          TYPE tabname,
    lv_whitelisted_only    TYPE abap_bool,
    lo_annotation          TYPE REF TO /clin/hcm_anno_shlp_annotation,
    lo_entity_type         TYPE REF TO /iwbep/if_mgw_odata_entity_typ,
    lo_property            TYPE REF TO /iwbep/if_mgw_odata_property,
    lt_search_helps        TYPE shlp_descr_tab_t,
    ld_search_help         TYPE REF TO shlp_descr_t,
    lv_qualifier           TYPE string,
    lv_default_help        TYPE shlp_descr_t-shlpname,
    lv_valuelist_entityset TYPE string,

    BEGIN OF ls_domain,
      search_help TYPE shlp_descr_t,
      property    TYPE /iwbep/if_mgw_med_odata_types=>ty_s_med_property,
    END OF ls_domain,
    lt_domains LIKE HASHED TABLE OF ls_domain WITH UNIQUE KEY search_help-shlpname.

*    SET RUN TIME ANALYZER ON.

  so_text_annotation = /clin/hcm_anno_text_annotation=>create( io_odata_model ).

  IF lines( it_fields ) > 0.
    lv_whitelisted_only = abap_true.
  ENDIF.

  " process structure binding of all entity types and complex types
  DATA(lt_types) = CAST /iwbep/if_mgw_odata_re_model( io_odata_model )->get_entity_types( ).
  DATA(lt_complex_types) = CAST /iwbep/if_mgw_odata_re_model( io_odata_model )->get_complex_types( ).
  APPEND LINES OF lt_complex_types TO lt_types.

  LOOP AT lt_types REFERENCE INTO DATA(ld_type) WHERE attribute_struct NP '*=>*'.
    lv_structname = ld_type->attribute_struct.

    " check whether to process this structure
    IF lv_whitelisted_only = abap_true.
      READ TABLE it_fields TRANSPORTING NO FIELDS WITH KEY tabname = lv_structname.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
    ENDIF.
    READ TABLE it_excluded_fields TRANSPORTING NO FIELDS WITH KEY tabname = lv_structname fieldname = space.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.

    " process all properties of the entity type
    LOOP AT ld_type->properties REFERENCE INTO DATA(ld_property).
      " check whether to process this structure component
      IF lv_whitelisted_only = abap_true.
        READ TABLE it_fields TRANSPORTING NO FIELDS WITH KEY tabname = lv_structname fieldname = ld_property->name.
        IF sy-subrc <> 0.
          READ TABLE it_fields TRANSPORTING NO FIELDS WITH KEY tabname = lv_structname fieldname = space.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.
      READ TABLE it_excluded_fields TRANSPORTING NO FIELDS WITH KEY tabname = lv_structname fieldname = ld_property->name.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      lt_search_helps = get_search_helps( iv_fieldname   = ld_property->name && ``
                                          iv_structname  = lv_structname ).
*      if ld_property->name = 'PRCTR'.
*        break-point.
*      endif.

      " we currently only support these four search help types
      DELETE lt_search_helps WHERE shlptype <> 'SH'
                               AND shlptype <> 'CH' AND shlptype <> 'CT'
                               AND shlptype <> 'FV'.

      " mark first search help that supports autosuggest as default => no qualifier
      READ TABLE lt_search_helps REFERENCE INTO ld_search_help WITH KEY intdescr-autosuggest = abap_true.
      IF sy-subrc = 0.
        lv_default_help = ld_search_help->shlpname.
      ELSE.
        READ TABLE lt_search_helps REFERENCE INTO ld_search_help INDEX 1.
        IF sy-subrc = 0.
          lv_default_help = ld_search_help->shlpname.
        ENDIF.
      ENDIF.

      LOOP AT lt_search_helps REFERENCE INTO ld_search_help.
        READ TABLE it_excluded_search_helps TRANSPORTING NO FIELDS WITH KEY table_line = ld_search_help->shlpname.
        IF sy-subrc = 0.
          CONTINUE.
        ENDIF.

        CASE ld_search_help->shlptype.
          WHEN 'SH' OR 'CH' OR 'CT'.
            add_entity_type( EXPORTING iv_annotated_entity_type = ld_type->external_name
                                       io_odata_model           = io_odata_model
                                       is_search_help           = ld_search_help->*
                             IMPORTING ev_valuelist_entityset   = lv_valuelist_entityset
                             CHANGING  ct_types                 = lt_types ).

            CHECK lv_valuelist_entityset IS NOT INITIAL.

            IF ld_search_help->shlpname = lv_default_help.
              CLEAR lv_qualifier.
            ELSE.
              lv_qualifier = odata_name( ld_search_help->shlpname ).
            ENDIF.

            lo_annotation = create_one( io_odata_model         = io_odata_model
                                        io_vocan_model         = io_vocan_model
                                        iv_namespace           = iv_namespace
                                        iv_entitytype          = ld_type->external_name && ``
                                        iv_property            = ld_property->external_name && ``
                                        iv_qualifier           = lv_qualifier
                                        iv_label               = ld_search_help->intdescr-ddtext
                                        iv_valuelist_entityset = lv_valuelist_entityset
                                        iv_search_supported    = ld_search_help->intdescr-autosuggest ).

            SORT ld_search_help->fieldprop BY shlplispos.
            LOOP AT ld_search_help->fieldprop REFERENCE INTO DATA(ld_fieldprop).
              " Structure field is NOT bound to search help parameter
              " - part of hit list                   => DisplayOnly
              " Structure field is bound to search help parameter
              " - input parameter or selection field => In
              " - output parameter or hit list field => Out
              " - both of the above                  => InOut
              READ TABLE ld_search_help->interface REFERENCE INTO DATA(ld_interface) WITH KEY shlpfield = ld_fieldprop->fieldname.
              IF sy-subrc <> 0.
                " not in interface of collective search help, so no structure binding
                IF ld_fieldprop->shlplispos <> '00'.
                  lo_annotation->add_display_parameter( odata_name( ld_fieldprop->fieldname ) ).
                ENDIF.
              ELSE.
                READ TABLE ld_type->properties REFERENCE INTO DATA(ld_local_data_property) WITH KEY name = ld_interface->valfield.
                IF sy-subrc <> 0.
                  " not bound to entity type property, so no field transport
                  IF ld_fieldprop->shlplispos <> '00'.
                    lo_annotation->add_display_parameter( odata_name( ld_fieldprop->fieldname ) ).
                  ENDIF.
                ELSE.
                  IF ld_fieldprop->shlpinput = abap_true OR ld_fieldprop->shlpselpos <> '00'.
                    " In or InOut
                    IF ld_fieldprop->shlpoutput = abap_true OR ld_fieldprop->shlplispos <> '00'.
                      lo_annotation->add_inout_parameter(
                        iv_property           = ld_local_data_property->external_name && ``
                        iv_valuelist_property = odata_name( ld_fieldprop->fieldname ) ).
                    ELSE.
                      lo_annotation->add_in_parameter(
                        iv_property           = ld_local_data_property->external_name && ``
                        iv_valuelist_property = odata_name( ld_fieldprop->fieldname ) ).
                    ENDIF.
                  ELSEIF ld_fieldprop->shlpoutput = abap_true OR ld_fieldprop->shlplispos <> '00'.
                    lo_annotation->add_out_parameter(
                      iv_property           = ld_local_data_property->external_name && ``
                      iv_valuelist_property = odata_name( ld_fieldprop->fieldname ) ).
                  ELSEIF ld_fieldprop->shlplispos <> '00'.
                    lo_annotation->add_display_parameter( odata_name( ld_fieldprop->fieldname ) ).
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDLOOP.

          WHEN 'FV'. " domain fixed values
            CHECK ld_property->core_type <> /iwbep/if_mgw_med_odata_types=>gcs_edm_data_types-boolean.
            /clin/hcm_anno_shlp_annotation=>create_one(
              io_odata_model         = io_odata_model
              io_vocan_model         = io_vocan_model
              iv_namespace           = iv_namespace
              iv_entitytype          = ld_type->external_name && ``
              iv_property            = ld_property->external_name && ``
              iv_label               = ld_search_help->intdescr-title
              iv_valuelist_entityset = entity_set_name( ld_search_help->* )
              iv_search_supported    = abap_false
            )->add_inout_parameter(
              iv_property            = ld_property->external_name && ``
              iv_valuelist_property  = 'Code' ##NO_TEXT
            )->add_display_parameter( 'Text' ).

            ls_domain-search_help = ld_search_help->*.
            ls_domain-property = ld_property->*.
            INSERT ls_domain INTO TABLE lt_domains.

        ENDCASE.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  LOOP AT lt_domains REFERENCE INTO DATA(ld_domain).
    lo_entity_type = io_odata_model->create_entity_type( entity_set_name( ld_domain->search_help ) && `` ).
    lo_entity_type->create_entity_set( entity_set_name( ld_domain->search_help ) && ``
    )->create_annotation( /iwbep/if_mgw_med_odata_types=>gc_sap_namespace
    )->add(
      iv_key   = 'semantics'     ##NO_TEXT
      iv_value = 'fixed-values'  ##NO_TEXT
    ).

    lo_property = lo_entity_type->create_property(
      iv_property_name  = 'Code'        ##NO_TEXT
      iv_abap_fieldname = 'DOMVALUE_L' ).
    lo_property->set_is_key( ).
    CASE ld_domain->property-core_type.
      WHEN /iwbep/if_mgw_med_odata_types=>gcs_edm_data_types-string.
        lo_property->set_type_edm_string( ).
        lo_property->set_maxlength( ld_domain->property-length ).
      WHEN /iwbep/if_mgw_med_odata_types=>gcs_edm_data_types-int32.
        lo_property->set_type_edm_int32( ).
      WHEN /iwbep/if_mgw_med_odata_types=>gcs_edm_data_types-int16.
        lo_property->set_type_edm_int16( ).
      WHEN /iwbep/if_mgw_med_odata_types=>gcs_edm_data_types-byte.
        lo_property->set_type_edm_byte( ).
      WHEN /iwbep/if_mgw_med_odata_types=>gcs_edm_data_types-sbyte.
        lo_property->set_type_edm_sbyte( ).
      WHEN /iwbep/if_mgw_med_odata_types=>gcs_edm_data_types-decimal.
        lo_property->set_type_edm_decimal( ).
        lo_property->set_maxlength( 2 * ld_domain->property-internal_length - 1 ).
        lo_property->set_precison( ld_domain->property-decimals ).
    ENDCASE.
    lo_property->set_internal_type( ld_domain->property-internal_type ).
    lo_property->set_internal_length( ld_domain->property-internal_length ).
    lo_property->/iwbep/if_mgw_odata_annotatabl~create_annotation(
      iv_annotation_namespace = /iwbep/if_mgw_med_odata_types=>gc_sap_namespace
    )->add(
      iv_key   = 'text'
      iv_value = 'Text' ##NO_TEXT
    ).
    lo_property->set_label_from_text_element(
      iv_text_element_symbol = 'VAL'
      iv_text_element_container = 'CL_O2C_FICA_SHLP_ANNOTATION'
    ).

    lo_property = lo_entity_type->create_property(
      iv_property_name  = 'Text'       ##NO_TEXT
      iv_abap_fieldname = 'DDTEXT' ).
    lo_property->set_internal_type( 'C' ).
    lo_property->set_maxlength( 60 ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( 60 ).
    lo_property->set_text_key(
      iv_text_key         = 'DD07V-DDTEXT'
      iv_text_object_type = 'B'
      iv_create           = abap_true ).

  ENDLOOP.

*    SET RUN TIME ANALYZER OFF.
ENDMETHOD.


  method CREATE_ONE.
    data:
      lo_ann_target type ref to /iwbep/if_mgw_vocan_ann_target,
      lo_annotation type ref to /iwbep/if_mgw_vocan_annotation,
      lo_record     type ref to /iwbep/if_mgw_vocan_record.

    " should have used builder pattern and passed this and the two models to constructor of builder
    if sv_namespace is initial.
      sv_namespace = iv_namespace.
      io_odata_model->set_schema_namespace( sv_namespace ).
    endif.

    lo_ann_target = io_vocan_model->create_annotations_target(
                      iv_target = sv_namespace && '.' && iv_entitytype && '/' && iv_property
                      iv_qualifier = iv_qualifier ).
    lo_annotation = lo_ann_target->create_annotation( iv_term = 'com.sap.vocabularies.Common.v1.ValueList' ).
    lo_record = lo_annotation->create_record( ).

    if iv_label is not initial.
      lo_record->create_property( 'Label' )->create_simple_value( )->set_string( iv_label && `` )        ##NO_TEXT.
    endif.

    lo_record->create_property( 'CollectionPath' )->create_simple_value( )->set_string( iv_valuelist_entityset ).

    if iv_valuelist_service_name is not initial.
      data(lv_path) = `../..`.
      if iv_valuelist_service_name(1) <> '/'.
        lv_path = lv_path && '/sap/'.
      endif.
      lv_path = lv_path && iv_valuelist_service_name.
      if iv_valuelist_service_version <> 1.
        lv_path = lv_path && |;v={ iv_valuelist_service_version alpha = out width = 1 }|.
      endif.
      lv_path = lv_path && '/'.
      lo_record->create_property( 'CollectionRoot' )->create_simple_value( )->set_string( lv_path ).
    endif.

    if iv_search_supported = abap_true.
      lo_record->create_property( 'SearchSupported' )->create_simple_value( )->set_boolean( abap_true ).
    endif.

    create object ro_annotation.
    ro_annotation->mo_parameters = lo_record->create_property( 'Parameters' )->create_collection( )      ##NO_TEXT.

  endmethod.


  method DDIC_NAME.
    rv_ddic_name = iv_odata_name.
    translate rv_ddic_name using 'x/'.
    translate rv_ddic_name to upper case.
  endmethod.


  method ENTITY_SET_NAME.
    rv_odata_name = entity_type_name( iv_shlpname = is_search_help-shlpname
                                      iv_shlptype = is_search_help-shlptype ).
  endmethod.


  method ENTITY_TYPE_NAME.
    rv_odata_name = 'VL_' && iv_shlptype && '_' && odata_name( iv_shlpname ).
  endmethod.


  method FIXED_VALUES.
    io_odata_model->get_entity_set( entity_type_name( iv_shlpname = iv_shlpname
                                                      iv_shlptype = iv_shlptype )
    )->create_annotation( /iwbep/if_mgw_med_odata_types=>gc_sap_namespace
    )->add( iv_key   = 'semantics'     ##NO_TEXT
            iv_value = 'fixed-values'  ##NO_TEXT
    ).
  endmethod.


  METHOD GET_SEARCH_HELPS.

    DATA: ls_top_search_help TYPE shlp_descr_t.

    CLEAR ls_top_search_help.
    CALL FUNCTION 'DD_SHLP_GET_HELPMETHOD'
      EXPORTING
        tabname   = iv_structname
        fieldname = iv_fieldname
      CHANGING
        shlp      = ls_top_search_help
      EXCEPTIONS
        OTHERS    = 1.

    CHECK sy-subrc = 0.

    IF ls_top_search_help-intdescr-issimple = abap_true.
      INSERT ls_top_search_help INTO TABLE rt_search_help.
    ELSE.
      " collective search help - resolve into elementary search helps
      CALL FUNCTION 'DD_SHLP_EXPAND_HELPMETHOD'
        EXPORTING
          shlp_top = ls_top_search_help
        IMPORTING
          shlp_tab = rt_search_help.
    ENDIF.

  ENDMETHOD.                                             "#EC CI_VALPAR


  method ODATA_NAME.
    rv_odata_name = iv_ddic_name.
    translate rv_odata_name to upper case.
    translate rv_odata_name using '/x'.
  endmethod.


  method PROPERTY_NAME.
    rv_property_name = odata_name( iv_fieldname ).
  endmethod.


  method SEARCH_HELP.
    check iv_entityset_name(6) = gc_ch
       or iv_entityset_name(6) = gc_ct
       or iv_entityset_name(6) = gc_fv
       or iv_entityset_name(6) = gc_sh.

    rs_search_help-shlpname = ddic_name( iv_entityset_name+6 ).
    rs_search_help-shlptype = iv_entityset_name+3(2).
  endmethod.


  method SEARCH_HELP_IS_USABLE.
    clear rv_is_usable.

    " search help must have at least one inout parameter
    " TODO: this logic is repeated in CREATE_FOR_ALL
      rv_is_usable = abap_true.
    check 1 = 2.
    loop at is_search_help-fieldprop reference into data(ld_fieldprop).
      " Structure field is NOT bound to search help parameter
      " - part of hit list                   => DisplayOnly
      " Structure field is bound to search help parameter
      " - input parameter or selection field => In
      " - output parameter or hit list field => Out
      " - both of the above                  => InOut
      read table is_search_help-interface reference into data(ld_interface) with key shlpfield = ld_fieldprop->fieldname.
      check sy-subrc = 0.
      " in interface of collective search help
*      check ld_fieldprop->shlpinput = abap_true or ld_fieldprop->shlpselpos <> '00'.
*      check ld_fieldprop->shlpoutput = abap_true or ld_fieldprop->shlplispos <> '00'.
      check ld_fieldprop->shlpinput = abap_true  or ld_fieldprop->shlplispos <> '00'.
      check ld_fieldprop->shlpoutput = abap_true or ld_fieldprop->shlpselpos <> '00'.
      " found an InOut parameter => usable
      rv_is_usable = abap_true.
    endloop.

  endmethod.
ENDCLASS.
