class /CLIN/HCM_ANNO_TEXT_ANNOTATION definition
  public
  final
  create public .

public section.

  methods GET_MAPPED_DTEL
    importing
      !IV_DATA_ELEMENT_NAME type ROLLNAME
    returning
      value(RV_DATA_ELEMENT_NAME) type ROLLNAME .
  methods FOR_ALL
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  class-methods CREATE
    importing
      !IO_ODATA_MODEL type ref to /IWBEP/IF_MGW_ODATA_MODEL
    returning
      value(RO_ANNOTATION) type ref to /CLIN/HCM_ANNO_TEXT_ANNOTATION .
  methods TEXT
    importing
      !IV_ENTITY_TYPE type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME
      !IV_CODE_PROPERTY type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME
      !IV_TEXT_PROPERTY type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  class-methods IS_ANNOTATED_BY_GW
    returning
      value(RV_ANNOTATED_BY_GW) type ABAP_BOOL .
  methods FOR_PROPERTY
    importing
      !IV_ENTITY_NAME type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME optional
      !IV_PROPERTY_NAME type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME optional
      !IO_PROPERTY type ref to /IWBEP/IF_MGW_ODATA_PROPERTY optional
      !IV_DATA_ELEMENT_NAME type ROLLNAME
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  protected section.
private section.

  types:
    begin of s_dtel_mapping,
      source type fieldname,
      target type rollname,
    end of s_dtel_mapping .
  types:
    t_dtel_mapping TYPE HASHED TABLE OF s_dtel_mapping WITH UNIQUE KEY source .

  data MO_MODEL type ref to /IWBEP/IF_MGW_ODATA_MODEL .
  constants GC_MAPPING_STRUCTURE_NAME type DDOBJNAME value 'FKK_TEXT_DATA_ELEMENTS' ##NO_TEXT.
  class-data GV_ANNOTATED_BY_GW type ABAP_BOOL .
  data MT_DTEL_MAPPINGS type T_DTEL_MAPPING .

  methods LOAD_MAPPING .
ENDCLASS.



CLASS /CLIN/HCM_ANNO_TEXT_ANNOTATION IMPLEMENTATION.


  method CREATE.

    create object ro_annotation.

    is_annotated_by_gw( ).

    " retrieve the mapping table and keep as hash table
    ro_annotation->load_mapping( ).

    ro_annotation->mo_model = io_odata_model.

  endmethod.


  METHOD FOR_ALL.
    DATA:
      lv_structname   TYPE ddobjname,
      lt_fields       TYPE STANDARD TABLE OF dfies,
      ls_field        TYPE dfies,
      lv_text_key     TYPE string,
      lo_property     TYPE REF TO /iwbep/if_mgw_odata_property,
      lv_anno_val     TYPE /iwbep/med_annotation_value.

    FIELD-SYMBOLS:
      <fs_dtel_mapping> TYPE s_dtel_mapping.

    " Prerequisite: the data element mapping table is already loaded
    " which is established when using the CREATE factory method to instantiate the
    " class.

    " process structure binding of all entities
    DATA(lt_types) = CAST /iwbep/if_mgw_odata_re_model( mo_model )->get_entity_types( ).

    LOOP AT lt_types REFERENCE INTO DATA(ld_type) WHERE attribute_struct NP '*=>*'.
      " retrieve field information of structure bound to the entity
      lv_structname = ld_type->attribute_struct.
      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          tabname   = lv_structname
          all_types = abap_true
        TABLES
          dfies_tab = lt_fields
        EXCEPTIONS
          OTHERS    = 1.
      CHECK sy-subrc = 0.

      " process all properties of an entity
      LOOP AT ld_type->properties REFERENCE INTO DATA(ld_property).
        lo_property = mo_model->get_entity_type( ld_type->external_name )->get_property( ld_property->external_name ).
        READ TABLE lt_fields WITH KEY fieldname = ld_property->name INTO ls_field.
        CHECK sy-subrc = 0.

        " check for whether data element exists as a field in structure referenced by gc_mapping_structure_name
        READ TABLE mt_dtel_mappings WITH TABLE KEY source = ls_field-rollname ASSIGNING <fs_dtel_mapping>.
        IF sy-subrc EQ 0.
          " mapping exists use target data element as source of texts.

          " overwrite label text key (default element type)
          CONCATENATE gc_mapping_structure_name '-' <fs_dtel_mapping>-source INTO lv_text_key. "#EC NOTEXT
          lo_property->set_text_key(
            iv_text_key = lv_text_key
            iv_text_object_type = 'B'                       "#EC NOTEXT
            iv_create = abap_true ).

          " check if GW is ran with an SP that already supports text annotation
          IF gv_annotated_by_gw EQ abap_true.
            CONCATENATE gc_mapping_structure_name '-' <fs_dtel_mapping>-source INTO lv_text_key. "#EC NOTEXT
            " heading text
            lo_property->set_text_key(
              iv_text_key = lv_text_key
              iv_text_object_type = 'B'                     "#EC NOTEXT
              iv_text_element_type = 'H'                    "#EC NOTEXT
              iv_create = abap_true ).

            " quickinfo text
            lo_property->set_text_key(
              iv_text_key = lv_text_key
              iv_text_object_type = 'B'                     "#EC NOTEXT
              iv_text_element_type = 'Q'                    "#EC NOTEXT
              iv_create = abap_true ).

            " F1 summary
            lo_property->set_text_key(
              iv_text_key = lv_text_key
              iv_text_object_type = 'B'                     "#EC NOTEXT
              iv_text_element_type = 'S' ).                 "#EC NOTEXT

            " F1 documentation
            lo_property->set_text_key(
              iv_text_key = lv_text_key
              iv_text_object_type = 'B'                     "#EC NOTEXT
              iv_text_element_type = 'D' ).                 "#EC NOTEXT
          ENDIF.

        ENDIF.

      ENDLOOP.
    ENDLOOP.

    " process structure binding of all complex types
    DATA(lt_complex_types) = CAST /iwbep/if_mgw_odata_re_model( mo_model )->get_complex_types( ).

    LOOP AT lt_complex_types REFERENCE INTO DATA(ld_complex_type) WHERE attribute_struct NP '*=>*'.
      " retrieve field information of structure bound to the entity
      lv_structname = ld_complex_type->attribute_struct.
      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          tabname   = lv_structname
          all_types = abap_true
        TABLES
          dfies_tab = lt_fields
        EXCEPTIONS
          OTHERS    = 1.
      CHECK sy-subrc = 0.

      " process all properties of an complex type
      LOOP AT ld_complex_type->properties REFERENCE INTO DATA(ld_complex_property).
        lo_property = mo_model->get_complex_type( ld_complex_type->external_name )->get_property( ld_complex_property->external_name ).
        READ TABLE lt_fields WITH KEY fieldname = ld_complex_property->name INTO ls_field.
        CHECK sy-subrc = 0.

        " check for whether data element exists as a field in structure referenced by gc_mapping_structure_name
        READ TABLE mt_dtel_mappings WITH TABLE KEY source = ls_field-rollname ASSIGNING <fs_dtel_mapping>.
        IF sy-subrc EQ 0.
          " mapping exists use target data element as source of texts.

          " overwrite label text key (default element type)
          CONCATENATE gc_mapping_structure_name '-' <fs_dtel_mapping>-source INTO lv_text_key. "#EC NOTEXT
          lo_property->set_text_key(
            iv_text_key = lv_text_key
            iv_text_object_type = 'B'                       "#EC NOTEXT
            iv_create = abap_true ).

          " check if GW is ran with an SP that already supports text annotation
          IF gv_annotated_by_gw EQ abap_true.
            CONCATENATE gc_mapping_structure_name '-' <fs_dtel_mapping>-source INTO lv_text_key. "#EC NOTEXT
            lo_property->set_text_key(
              iv_text_key = lv_text_key
              iv_text_object_type = 'B'                     "#EC NOTEXT
              iv_text_element_type = 'H'                    "#EC NOTEXT
              iv_create = abap_true ).

            CONCATENATE gc_mapping_structure_name '-' <fs_dtel_mapping>-source INTO lv_text_key. "#EC NOTEXT
            lo_property->set_text_key(
              iv_text_key = lv_text_key
              iv_text_object_type = 'B'                     "#EC NOTEXT
              iv_text_element_type = 'Q'                    "#EC NOTEXT
              iv_create = abap_true ).
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD FOR_PROPERTY.

    DATA: lv_text_key TYPE        string,
          lo_entity   TYPE REF TO /iwbep/if_mgw_odata_entity_typ,
          lo_property TYPE REF TO /iwbep/if_mgw_odata_property.

    IF ( io_property IS SUPPLIED ).
      lo_property = io_property.
    ELSE.
      IF ( iv_entity_name IS SUPPLIED AND iv_property_name IS SUPPLIED ).
        lo_entity = mo_model->get_entity_type( iv_entity_name ).
        lo_property = lo_entity->get_property( iv_property_name ).
      ENDIF.
    ENDIF.

    lv_text_key = iv_data_element_name.

    " label text
    lo_property->set_text_key(
      iv_text_key = lv_text_key
      iv_text_object_type = 'B'                             "#EC NOTEXT
      iv_text_element_type = 'L' ).                         "#EC NOTEXT

    " check if GW is ran with an SP that already supports text annotation
    IF gv_annotated_by_gw EQ abap_true.
      " heading text
      lo_property->set_text_key(
        iv_text_key = lv_text_key
        iv_text_object_type = 'B'                           "#EC NOTEXT
        iv_text_element_type = 'H' ).                       "#EC NOTEXT

      " quickinfo text
      lo_property->set_text_key(
        iv_text_key = lv_text_key
        iv_text_object_type = 'B'                           "#EC NOTEXT
        iv_text_element_type = 'Q' ).                       "#EC NOTEXT

      " F1 summary
      lo_property->set_text_key(
        iv_text_key = lv_text_key
        iv_text_object_type = 'B'                           "#EC NOTEXT
        iv_text_element_type = 'S' ).                       "#EC NOTEXT

      " F1 documentation
      " F1 summary
      lo_property->set_text_key(
        iv_text_key = lv_text_key
        iv_text_object_type = 'B'                           "#EC NOTEXT
        iv_text_element_type = 'D' ).                       "#EC NOTEXT

    ENDIF.

  ENDMETHOD.


  method GET_MAPPED_DTEL.

    field-symbols:
          <fs_dtel_mapping> type s_dtel_mapping.

    " return input data element as default (no mapping)
    rv_data_element_name = iv_data_element_name.

    if ( mt_dtel_mappings is not initial ).
      " check whether data element exists in the mapping table already read from mapping DDIC structure
      read table mt_dtel_mappings with table key source = iv_data_element_name assigning <fs_dtel_mapping>.

      if sy-subrc eq 0.
        " mapping exists use target data element as source of texts.
        rv_data_element_name = <fs_dtel_mapping>-target.
      endif.
    endif.

  endmethod.


  METHOD IS_ANNOTATED_BY_GW.

    DATA: lr_if_descriptor TYPE REF TO cl_abap_intfdescr,
          lr_s_descriptor  TYPE REF TO cl_abap_structdescr,
          lt_components    TYPE cl_abap_structdescr=>component_table.

    IF gv_annotated_by_gw IS INITIAL.

      lr_if_descriptor ?= cl_abap_intfdescr=>describe_by_name( p_name = '/IWBEP/IF_MGW_MED_ODATA_TYPES' ). "#EC NOTEXT
      lr_s_descriptor ?= lr_if_descriptor->get_attribute_type( p_name =  'GCS_TEXT_E_TYPE' ). "#EC NOTEXT
      READ TABLE lr_s_descriptor->components WITH TABLE KEY name = 'HEADING' TRANSPORTING NO FIELDS. "#EC NOTEXT
      IF sy-subrc = 0.
        gv_annotated_by_gw = abap_true.
      ELSE.
        gv_annotated_by_gw = abap_false.
      ENDIF.

    ENDIF.
    rv_annotated_by_gw = gv_annotated_by_gw.

  ENDMETHOD.


  method LOAD_MAPPING.

    data: lt_dtel_mapping type standard table of dfies with key fieldname,
          ls_dtel_mapping type s_dtel_mapping.

    field-symbols:
          <fs_dtel_mapping> type dfies.

    " retrieve information of structure containing the data element mappings
    " field name must be equal to the name of the source data element
    " data element must be set to the target data element.
    call function 'DDIF_FIELDINFO_GET'
      exporting
        tabname   = gc_mapping_structure_name
        all_types = abap_true
      tables
        dfies_tab = lt_dtel_mapping
      exceptions
        others    = 1.

    check sy-subrc = 0.

    loop at lt_dtel_mapping assigning <fs_dtel_mapping>.
      clear ls_dtel_mapping.
      ls_dtel_mapping-source = <fs_dtel_mapping>-fieldname.
      ls_dtel_mapping-target = <fs_dtel_mapping>-rollname.

      insert ls_dtel_mapping into table mt_dtel_mappings.
    endloop.

  endmethod.


  method TEXT.
    mo_model->get_entity_type( iv_entity_type )->get_property( iv_code_property )->/iwbep/if_mgw_odata_annotatabl~create_annotation(
      iv_annotation_namespace = /iwbep/if_mgw_med_odata_types=>gc_sap_namespace
    )->add(
      iv_key   = 'text'                                     "#EC NOTEXT
      iv_value = `` && iv_text_property
    ).
  endmethod.
ENDCLASS.
