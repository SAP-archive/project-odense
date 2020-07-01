*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_abapgit_provider IMPLEMENTATION.

  METHOD lif_abapgit_provider~validate_package.
    cl_abapgit_repo_srv=>get_instance( )->validate_package( iv_package = iv_package ).
  ENDMETHOD.

  METHOD lif_abapgit_provider~list_repositories.
    rt_list = cl_abapgit_repo_srv=>get_instance( )->list( ).
  ENDMETHOD.

  METHOD lif_abapgit_provider~validate_transport_request.

    DATA: lv_error_message TYPE string.

    SELECT SINGLE trstatus FROM e070 INTO @DATA(lv_trstatus)
      WHERE
        trkorr = @iv_transport_request.

    IF sy-subrc NE 0.
      MESSAGE e003(a4c_agit_adt) WITH iv_transport_request INTO lv_error_message.
      cx_abapgit_exception=>raise_t100( ).
    ELSEIF lv_trstatus NE 'D'.
      MESSAGE e004(a4c_agit_adt) WITH iv_transport_request INTO lv_error_message.
      cx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_abapgit_provider~set_authentication_info.
    cl_abapgit_default_auth_info=>refresh( ).
    cl_abapgit_default_auth_info=>set_auth_info( iv_user     = iv_user
                                                 iv_password = iv_password ).
  ENDMETHOD.

  METHOD lif_abapgit_provider~perform_import.

    "Set the default transport request
    IF is_request_data-transportrequest IS NOT INITIAL.
      cl_abapgit_default_transport=>get_instance( )->set( CONV #( is_request_data-transportrequest ) ).
    ENDIF.

    "Create online repo
    DATA(lo_repo) = cl_abapgit_repo_srv=>get_instance( )->new_online(
         iv_url         = is_request_data-url
         iv_branch_name = is_request_data-branch
         iv_package     = CONV devclass( is_request_data-package ) ).

    "Pull objects
    lo_repo->refresh( ).

    DATA(ls_checks) = lo_repo->deserialize_checks( ).

    "Overwrite existing objects
    LOOP AT ls_checks-overwrite ASSIGNING FIELD-SYMBOL(<ls_overwrite>).
      <ls_overwrite>-decision = 'Y'.
    ENDLOOP.

    LOOP AT ls_checks-warning_package ASSIGNING FIELD-SYMBOL(<ls_warning_package>).
      <ls_warning_package>-decision = 'Y'.
    ENDLOOP.

    "Import objects
    ls_checks-transport-transport = is_request_data-transportrequest.
    lo_repo->deserialize( is_checks = ls_checks
                          ii_log    = ii_log ).

  ENDMETHOD.

  METHOD lif_abapgit_provider~link_repo.

    "Create online repo
    DATA(lo_repo) = cl_abapgit_repo_srv=>get_instance( )->new_online(
         iv_url         = is_request_data-url
         iv_branch_name = is_request_data-branch
         iv_package     = CONV devclass( is_request_data-package ) ).

  ENDMETHOD.

  METHOD lif_abapgit_provider~is_tr_check_required.

    DATA: ls_tr_check_data TYPE transport_check.

    rv_is_required = abap_true.

    ls_tr_check_data-pgmid      = 'R3TR'.
    ls_tr_check_data-object     = 'DEVC'.
    ls_tr_check_data-objectname = iv_package.
    ls_tr_check_data-operation  = 'I'.
    cl_cts_adt_obj_record=>check_objects( CHANGING cs_transport_check = ls_tr_check_data ).

    IF ls_tr_check_data-recording = abap_false AND ls_tr_check_data-result <> 'E'.
      CLEAR: rv_is_required.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
