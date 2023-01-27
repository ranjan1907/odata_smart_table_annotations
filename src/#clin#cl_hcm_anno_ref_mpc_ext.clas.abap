class /CLIN/CL_HCM_ANNO_REF_MPC_EXT definition
  public
  inheriting from /CLIN/CL_HCM_ANNO_REF_MPC
  create public .

public section.

  methods DEFINE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /CLIN/CL_HCM_ANNO_REF_MPC_EXT IMPLEMENTATION.


  METHOD define.

* required to call define method of base model provider class first which is generated by service builder
    super->define( ).



    /clin/hcm_anno_text_annotation=>create( model )->for_all( ).


    /clin/hcm_anno_shlp_annotation=>create_for_all(
            io_odata_model         = model
            io_vocan_model         = vocab_anno_model
            iv_namespace           = 'HCM_ANNO_REF_SRV' ).


  ENDMETHOD.
ENDCLASS.
