class /CLIN/CL_HCM_ANNO_REF definition
  public
  final
  create public .

public section.

  class-methods UPDATE_CUSTOM_TABLE .
  methods GET_ENTITY
    importing
      !IV_PERNR type /CLIN/CL_HCM_ANNO_REF_MPC=>TS_EMPLOYEEDATA-PERNR
    exporting
      !ER_OUTPUT type /CLIN/CL_HCM_ANNO_REF_MPC=>TS_EMPLOYEEDATA
      !ET_RETURN type BAPIRET2_T .
  methods GET_ENTITYSET
    importing
      !IR_PERNR type /IWBEP/T_COD_SELECT_OPTIONS
      !IR_ENAME type /IWBEP/T_COD_SELECT_OPTIONS
      !IR_BUKRS type /IWBEP/T_COD_SELECT_OPTIONS
      !IR_WERKS type /IWBEP/T_COD_SELECT_OPTIONS
      !IR_PERSG type /IWBEP/T_COD_SELECT_OPTIONS
      !IR_PERSK type /IWBEP/T_COD_SELECT_OPTIONS
      !IR_BTRTL type /IWBEP/T_COD_SELECT_OPTIONS
      !IR_GESCH type /IWBEP/T_COD_SELECT_OPTIONS
      !IR_LAND1 type /IWBEP/T_COD_SELECT_OPTIONS
      !IR_BUKRS_NEW type /IWBEP/T_COD_SELECT_OPTIONS
      !IR_GESCH_NEW type /IWBEP/T_COD_SELECT_OPTIONS
      !IR_LAND1_NEW type /IWBEP/T_COD_SELECT_OPTIONS
      !IR_LOCATION type /IWBEP/T_COD_SELECT_OPTIONS
    exporting
      !ET_OUTPUT type /CLIN/CL_HCM_ANNO_REF_MPC=>TT_EMPLOYEEDATA
      !ET_RETURN type BAPIRET2_T .
  methods CREATE_ENTITY
    importing
      !IS_INPUT type /CLIN/CL_HCM_ANNO_REF_MPC=>TS_EMPLOYEEDATA
    exporting
      !ER_ENTITY type /CLIN/CL_HCM_ANNO_REF_MPC=>TS_EMPLOYEEDATA
      !ET_RETURN type BAPIRET2_T .
  methods UPDATE_ENTITY
    importing
      !IS_INPUT type /CLIN/CL_HCM_ANNO_REF_MPC=>TS_EMPLOYEEDATA
    exporting
      !ER_ENTITY type /CLIN/CL_HCM_ANNO_REF_MPC=>TS_EMPLOYEEDATA
      !ET_RETURN type BAPIRET2_T .
  methods DELETE_ENTITY
    importing
      !IV_PERNR type /CLIN/CL_HCM_ANNO_REF_MPC=>TS_EMPLOYEEDATA-PERNR
    exporting
      !ET_RETURN type BAPIRET2_T .
protected section.
private section.
ENDCLASS.



CLASS /CLIN/CL_HCM_ANNO_REF IMPLEMENTATION.


METHOD create_entity.

  DATA: ls_error    TYPE bapiret2,
        ls_emp_data TYPE /clin/emp_data.

  MOVE-CORRESPONDING is_input TO ls_emp_data.
  ls_emp_data-pernr_key = ls_emp_data-pernr.

  INSERT /clin/emp_data FROM ls_emp_data.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    ls_error-type = 'E'.
    ls_error-id = '00'.
    ls_error-number = '001'.
    ls_error-message_v1 = 'Database Create Failed'.
    APPEND ls_error TO et_return.
    CLEAR: ls_error.
  ENDIF.

ENDMETHOD.


METHOD delete_entity.

  DATA: ls_error    TYPE bapiret2,
        ls_emp_data TYPE /clin/emp_data.

  ls_emp_data-pernr_key = iv_pernr.

  DELETE /clin/emp_data FROM ls_emp_data.

  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    ls_error-type = 'E'.
    ls_error-id = '00'.
    ls_error-number = '001'.
    ls_error-message_v1 = 'Database Delete Failed'.
    APPEND ls_error TO et_return.
    CLEAR: ls_error.
  ENDIF.

ENDMETHOD.


METHOD get_entity.

  DATA: ls_error TYPE bapiret2.

  IF NOT iv_pernr IS INITIAL.
    SELECT SINGLE * FROM /clin/emp_data INTO CORRESPONDING FIELDS OF er_output WHERE pernr_key = iv_pernr.
    IF sy-subrc <> 0.
      CLEAR: er_output.
      ls_error-type = 'E'.
      ls_error-id = '00'.
      ls_error-number = '001'.
      ls_error-message_v1 = 'Database Select Failed'.
      APPEND ls_error TO et_return.
      CLEAR: ls_error.
    ENDIF.
  ELSE.
    ls_error-type = 'E'.
    ls_error-id = '00'.
    ls_error-number = '001'.
    ls_error-message_v1 = 'No Emp ID Passed - Select Failed'.
    APPEND ls_error TO et_return.
    CLEAR: ls_error.
  ENDIF.

ENDMETHOD.


METHOD get_entityset.

  DATA: ls_error TYPE bapiret2.

  SELECT * FROM /clin/emp_data INTO CORRESPONDING FIELDS OF TABLE et_output
    WHERE pernr_key IN ir_pernr
      AND ename IN ir_ename
      AND bukrs IN ir_bukrs
      AND werks IN ir_werks
      AND persg IN ir_persg
      AND persk IN ir_persk
      AND btrtl IN ir_btrtl
      AND gesch IN ir_gesch
      AND land1 IN ir_land1
      AND bukrs_new IN ir_bukrs_new
      AND gesch_new IN ir_gesch_new
      AND land1_new IN ir_land1_new
      AND location IN ir_location.
  IF sy-subrc <> 0.
    CLEAR: et_output.
    ls_error-type = 'E'.
    ls_error-id = '00'.
    ls_error-number = '001'.
    ls_error-message_v1 = 'Database Select Failed'.
    APPEND ls_error TO et_return.
    CLEAR: ls_error.
  ENDIF.

ENDMETHOD.


  METHOD update_custom_table.


    DATA: lt_emp_data TYPE STANDARD TABLE OF /clin/emp_data.

    SELECT a~pernr a~begda a~endda
           b~gesch
           a~ename a~bukrs a~werks a~btrtl a~persg a~persk
      INTO CORRESPONDING FIELDS OF TABLE lt_emp_data
      FROM pa0001 AS a INNER JOIN pa0002 AS b ON a~pernr = b~pernr
      WHERE a~pernr LE '02835000'
        AND a~begda LE sy-datum AND a~endda GE sy-datum
        AND b~begda LE sy-datum AND b~endda GE sy-datum.

    SORT lt_emp_data BY ename.

    LOOP AT lt_emp_data ASSIGNING FIELD-SYMBOL(<ls_emp_data>).

      IF sy-tabix LT 100.
        <ls_emp_data>-gesch_new = '0'.
        <ls_emp_data>-bukrs_new = <ls_emp_data>-bukrs.
        <ls_emp_data>-land1_new = 'DE'.
        <ls_emp_data>-location = 'Germany'.

      ELSEIF sy-tabix LT 200.

        <ls_emp_data>-gesch_new = 1.
        <ls_emp_data>-bukrs_new = <ls_emp_data>-bukrs.
        <ls_emp_data>-land1_new = 'IN'.
        <ls_emp_data>-location = 'Gurgaon'.

      ELSEIF sy-tabix LT 300.

        <ls_emp_data>-gesch_new = 2.
        <ls_emp_data>-bukrs_new = <ls_emp_data>-bukrs.
        <ls_emp_data>-land1_new = 'CH'.
        <ls_emp_data>-location = 'Noida'.

      ELSEIF sy-tabix LT 400.
        <ls_emp_data>-gesch_new = 3.
        <ls_emp_data>-bukrs_new = <ls_emp_data>-bukrs.
        <ls_emp_data>-land1_new = 'GR'.
        <ls_emp_data>-location = 'Bangalore'.

      ELSEIF sy-tabix LT 500.
        <ls_emp_data>-gesch_new = 4.
        <ls_emp_data>-bukrs_new = <ls_emp_data>-bukrs.
        <ls_emp_data>-land1_new = 'UK'.
        <ls_emp_data>-location = 'Pune'.
      ENDIF.

      <ls_emp_data>-pernr_key = <ls_emp_data>-pernr.
      <ls_emp_data>-land1 = <ls_emp_data>-bukrs+0(2).

    ENDLOOP.

    SORT lt_emp_data BY pernr_key.

    BREAK-POINT.

    INSERT /clin/emp_data FROM TABLE lt_emp_data ACCEPTING DUPLICATE KEYS.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


METHOD update_entity.

  DATA: ls_error    TYPE bapiret2,
        ls_emp_data TYPE /clin/emp_data.

  MOVE-CORRESPONDING is_input TO ls_emp_data.
  ls_emp_data-pernr_key = ls_emp_data-pernr.

  UPDATE /clin/emp_data FROM ls_emp_data.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    ls_error-type = 'E'.
    ls_error-id = '00'.
    ls_error-number = '001'.
    ls_error-message_v1 = 'Database Update Failed'.
    APPEND ls_error TO et_return.
    CLEAR: ls_error.
  ENDIF.

ENDMETHOD.
ENDCLASS.
