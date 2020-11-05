CLASS cl_abapgit_res_repo_pull DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_request_pull_data,
        branch           TYPE string,
        transportrequest TYPE string,
        user             TYPE string,
        password         TYPE string,
      END OF ty_request_pull_data.

    CONSTANTS co_st_name_pull           TYPE string     VALUE 'ABAPGIT_ST_REPO_PULL' ##NO_TEXT.
    CONSTANTS co_root_name_pull         TYPE string     VALUE 'REPOSITORY' ##NO_TEXT.
    CONSTANTS co_content_type_repo_v1   TYPE string     VALUE 'application/abapgit.adt.repo.v1+xml' ##NO_TEXT.
    CONSTANTS co_content_type_repos_v1  TYPE string     VALUE 'application/abapgit.adt.repos.v1+xml' ##NO_TEXT.
    CONSTANTS co_st_name_pull_v2        TYPE string     VALUE 'ABAPGIT_ST_REPO_PULL_V2' ##NO_TEXT.
    CONSTANTS co_content_type_repo_v3   TYPE string     VALUE 'application/abapgit.adt.repo.v3+xml' ##NO_TEXT.
    CONSTANTS co_content_type_repos_v2  TYPE string     VALUE 'application/abapgit.adt.repos.v2+xml' ##NO_TEXT.

    METHODS post REDEFINITION.
    METHODS get  REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS cl_abapgit_res_repo_pull IMPLEMENTATION.


  METHOD get.

    DATA(ls_requested_content_type) =
      request->get_inner_rest_request( )->get_header_field( if_http_header_fields=>content_type ).

    " case co_content_type_repos_v2 to handle pull for emf model of abapGit Repositories view.

    CASE ls_requested_content_type.
      WHEN co_content_type_repos_v1.
        DATA(lo_resp_content_handler) =
          cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_plain_text(
                                                                       content_type      = co_content_type_repos_v1
                                                                       strict_conversion = abap_true ).
      WHEN co_content_type_repos_v2.
        lo_resp_content_handler =
          cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_plain_text(
                                                                       content_type      = co_content_type_repos_v2
                                                                       strict_conversion = abap_true ).

      WHEN OTHERS.
        response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).

    ENDCASE.



    " validation of request 'Accept:' header
    cl_adt_rest_comp_cnt_handler=>create( request = request
                                          content_handler = lo_resp_content_handler )->check_cnt_type_is_supported( ).

    TRY.
        response->set_body_data(
                  content_handler = lo_resp_content_handler
                  data            = |DEMO STATUS| ).

      CATCH cx_st_error cx_abapgit_exception INTO DATA(lx_error).
        cx_adt_rest_abapgit=>raise_with_error(
            ix_error       = lx_error
            iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
    ENDTRY.


  ENDMETHOD.


  METHOD post.

    DATA:
      ls_request_data TYPE ty_request_pull_data,
      lv_repo_key     TYPE if_abapgit_persistence=>ty_value.

    TRY.
        " Get Repository Key
        request->get_uri_attribute( EXPORTING
                                      name = 'key'
                                      mandatory = abap_true
                                    IMPORTING
                                      value = lv_repo_key ).

        " case co_content_type_repo_v3 to handle pull request for emf model of abapGit Repositories view.

        DATA(ls_requested_content_type) =
          request->get_inner_rest_request( )->get_header_field( if_http_header_fields=>content_type ).

        CASE ls_requested_content_type.
          WHEN co_content_type_repo_v1.
            DATA(lo_request_content_handler) = cl_adt_rest_comp_cnt_handler=>create(
                request         = request
                content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                                      st_name      = co_st_name_pull
                                      root_name    = co_root_name_pull
                                      content_type = co_content_type_repo_v1 ) ).

          WHEN co_content_type_repo_v3.
            lo_request_content_handler = cl_adt_rest_comp_cnt_handler=>create(
                request         = request
                content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                                      st_name      = co_st_name_pull_v2
                                      root_name    = co_root_name_pull
                                      content_type = co_content_type_repo_v3 ) ).

          WHEN OTHERS.
            response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
        ENDCASE.

        " Retrieve request data
        request->get_body_data(
          EXPORTING
            content_handler = lo_request_content_handler
          IMPORTING
            data            = ls_request_data ).

        " Trigger pull as background job
        DATA(lo_bg_action) =
          cl_abapgit_bg_action_factory=>get_action_instance( iv_repo_key = lv_repo_key
                                                             iv_action   = if_abapgit_app_log=>c_action_pull ).

        " define parameter for background job
        DATA(lv_type) = lo_bg_action->get_param_type( ).
        DATA ls_param TYPE REF TO data.
        FIELD-SYMBOLS <lv_field> TYPE any.
        CREATE DATA ls_param TYPE (lv_type).
        ASSIGN ls_param->* TO FIELD-SYMBOL(<ls_param>).

        ASSIGN COMPONENT 'BRANCH'           OF STRUCTURE <ls_param> TO <lv_field>.
        <lv_field> = ls_request_data-branch.
        ASSIGN COMPONENT 'TRANSPORTREQUEST' OF STRUCTURE <ls_param> TO <lv_field>.
        <lv_field> = ls_request_data-transportrequest.
        lo_bg_action->set_param( ir_param = REF #( <ls_param> ) ).

        " provide repository credentials
        DATA ls_credentials TYPE tsa4c_abapgit_credentials.
        ls_credentials-user     = ls_request_data-user.
        ls_credentials-password = ls_request_data-password.
        lo_bg_action->set_credentials( ls_credentials ).

        " schedule pull
        lo_bg_action->schedule_job( ).

        response->set_status( cl_rest_status_code=>gc_success_accepted ).

      " Handle issues
      CATCH cx_abapgit_bg_action_running INTO DATA(lx_bg_action_running).
        cx_adt_rest_abapgit=>raise_with_error(
            ix_error       = lx_bg_action_running
            iv_http_status = cl_rest_status_code=>gc_client_error_conflict ). "409
      CATCH cx_abapgit_exception cx_abapgit_app_log cx_a4c_logger cx_cbo_job_scheduler cx_uuid_error
          cx_abapgit_not_found INTO DATA(lx_exception).
        ROLLBACK WORK.
        cx_adt_rest_abapgit=>raise_with_error(
            ix_error       = lx_exception
            iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
