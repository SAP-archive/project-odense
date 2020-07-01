CLASS cl_abapgit_res_repo_push DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS post
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS prepare_action_param
      IMPORTING
        is_request_data    TYPE cl_abapgit_res_repo_stage=>ty_abapgit_staging
      EXPORTING
        et_staged_objects  TYPE tta4c_abapgit_object
        es_abapgit_comment TYPE tsa4c_abapgit_comment
      RAISING
        cx_abapgit_exception .
ENDCLASS.



CLASS cl_abapgit_res_repo_push IMPLEMENTATION.


  METHOD post.

    DATA:
      lv_repo_key     TYPE if_abapgit_persistence=>ty_value,
      lv_username     TYPE string,
      ls_request_data TYPE cl_abapgit_res_repo_stage=>ty_abapgit_staging,
      lo_repo_online  TYPE REF TO cl_abapgit_repo_online.

    TRY.
        " Handle request data
        " Get repository key
        request->get_uri_attribute( EXPORTING name = 'key' mandatory = abap_true
                                    IMPORTING value = lv_repo_key ).

        " Content Handler
        DATA(lo_request_content_handler) = cl_adt_rest_comp_cnt_handler=>create(
            request         = request
            content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                                  st_name      = cl_abapgit_res_repo_stage=>co_st_name
                                  root_name    = cl_abapgit_res_repo_stage=>co_root_name
                                  content_type = cl_abapgit_res_repo_stage=>co_content_type_v1 ) ).

        " Retrieve request data
        request->get_body_data(
          EXPORTING
            content_handler = lo_request_content_handler
          IMPORTING
            data            = ls_request_data ).

        " Get credentials from request header
        lv_username = request->get_inner_rest_request( )->get_header_field( iv_name = 'Username' ).

        " Client encodes password with base64 algorithm
        DATA(lv_password) = cl_abapgit_res_util=>encode_password(
          request->get_inner_rest_request( )->get_header_field( iv_name = 'Password' ) ).

        " Determine repo specific data
        cl_abapgit_factory=>get_environment( )->set_repo_action( if_abapgit_app_log=>c_action_push ).
        DATA(lo_repo) = cl_abapgit_repo_srv=>get_instance( )->get( lv_repo_key ).
        lo_repo_online ?= lo_repo.
        DATA(lv_repo_branch) = lo_repo_online->get_branch_name( ).

        " Trigger push as background job
        DATA(lo_bg_action) =
          cl_abapgit_bg_action_factory=>get_action_instance( iv_repo_key = lv_repo_key
                                                             iv_action   = if_abapgit_app_log=>c_action_push ).

        " define parameter for background job
        DATA(lv_type) = lo_bg_action->get_param_type( ).
        DATA ls_param TYPE REF TO data.
        FIELD-SYMBOLS <ls_commit> TYPE any.
        CREATE DATA ls_param TYPE (lv_type).
        ASSIGN ls_param->* TO FIELD-SYMBOL(<ls_param>).

        prepare_action_param( EXPORTING is_request_data    = ls_request_data
                              IMPORTING et_staged_objects  = DATA(lt_staged_objects)
                                        es_abapgit_comment = DATA(ls_abapgit_comment) ).
        ASSIGN COMPONENT 'STAGED_OBJECTS' OF STRUCTURE <ls_param> TO FIELD-SYMBOL(<lt_objects>).
        <lt_objects> = lt_staged_objects.
        ASSIGN COMPONENT 'ABAPGIT_COMMENT' OF STRUCTURE <ls_param> TO FIELD-SYMBOL(<ls_comment>).
        <ls_comment> = ls_abapgit_comment.
        lo_bg_action->set_param( ir_param = REF #( <ls_param> ) ).

        " provide repository credentials
        DATA ls_credentials TYPE tsa4c_abapgit_credentials.
        ls_credentials-user     = lv_username.
        ls_credentials-password = lv_password.
        lo_bg_action->set_credentials( ls_credentials ).

        " schedule push
        lo_bg_action->schedule_job( ).

        response->set_status( cl_rest_status_code=>gc_success_accepted ).

      " Handle issues
      CATCH cx_abapgit_bg_action_running INTO DATA(lx_bg_action_running).
        cx_adt_rest_abapgit=>raise_with_error(
            ix_error       = lx_bg_action_running
            iv_http_status = cl_rest_status_code=>gc_client_error_conflict ). "409
      CATCH cx_abapgit_exception INTO DATA(lx_exception).
        ROLLBACK WORK.
        cx_adt_rest_abapgit=>raise_with_error(
            ix_error       = lx_exception
            iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
    ENDTRY.

  ENDMETHOD.


  METHOD prepare_action_param.

    CLEAR et_staged_objects.
    CLEAR es_abapgit_comment.

    DATA:
      ls_staged_objects_transformed LIKE LINE OF et_staged_objects,
      ls_staged_objects_files       LIKE LINE OF ls_staged_objects_transformed-files.

    " Node: STAGED_OBJECTS
    LOOP AT is_request_data-staged_objects ASSIGNING FIELD-SYMBOL(<ls_staged_objects>).
      CLEAR: ls_staged_objects_transformed, ls_staged_objects_files.
      ls_staged_objects_transformed-object_ref = <ls_staged_objects>-object_ref.
      LOOP AT <ls_staged_objects>-files ASSIGNING FIELD-SYMBOL(<ls_staged_objects_files>).
        ls_staged_objects_files-filename    = <ls_staged_objects_files>-filename.
        ls_staged_objects_files-path        = <ls_staged_objects_files>-path.
        ls_staged_objects_files-localstate  = <ls_staged_objects_files>-localstate.
        ls_staged_objects_files-remotestate = <ls_staged_objects_files>-remotestate.
        INSERT ls_staged_objects_files INTO TABLE ls_staged_objects_transformed-files.
      ENDLOOP.
      INSERT ls_staged_objects_transformed INTO TABLE et_staged_objects.
    ENDLOOP.

    " Node: ABAPGIT_COMMENT
    es_abapgit_comment = is_request_data-abapgit_comment.

  ENDMETHOD.

ENDCLASS.
