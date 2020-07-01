CLASS cl_abapgit_res_repo_info_ext DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_request_data,
        url      TYPE string,
        user     TYPE string,
        password TYPE string,
      END OF ty_request_data.
    TYPES:
      BEGIN OF ty_response_data,
        access_mode TYPE string,
        branches    TYPE if_abapgit_definitions=>ty_git_branch_list_tt,
      END OF ty_response_data.


    CONSTANTS co_content_type_request_v1  TYPE string VALUE 'application/abapgit.adt.repo.info.ext.request.v1+xml' ##NO_TEXT.
    CONSTANTS co_content_type_response_v1 TYPE string VALUE 'application/abapgit.adt.repo.info.ext.response.v1+xml' ##NO_TEXT.
    CONSTANTS co_root_name_request        TYPE string VALUE 'REPOSITORY_EXTERNAL_REQ' ##NO_TEXT.
    CONSTANTS co_st_name_request          TYPE string VALUE 'ABAPGIT_ST_REPO_INFO_EXT_REQ' ##NO_TEXT.
    CONSTANTS co_st_name_response         TYPE string VALUE 'ABAPGIT_ST_REPO_INFO_EXT_RES' ##NO_TEXT.
    CONSTANTS co_root_name_response       TYPE string VALUE 'REPOSITORY_EXTERNAL' ##NO_TEXT.
    CONSTANTS co_content_type_request_v2  TYPE string VALUE 'application/abapgit.adt.repo.info.ext.request.v2+xml' ##NO_TEXT.
    CONSTANTS co_content_type_response_v2 TYPE string VALUE 'application/abapgit.adt.repo.info.ext.response.v2+xml' ##NO_TEXT.
    CONSTANTS co_st_name_request_v2       TYPE string VALUE 'ABAPGIT_ST_REPO_INFO_EXT_RQ_V2' ##NO_TEXT.
    CONSTANTS co_st_name_response_v2      TYPE string VALUE 'ABAPGIT_ST_REPO_INFO_EXT_RS_V2' ##NO_TEXT.

    METHODS post REDEFINITION.


  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: mo_repo_auth_srv TYPE REF TO if_abapgit_repository_auth,
          mt_branches TYPE if_abapgit_definitions=>ty_git_branch_list_tt.

    METHODS:
      get_repo_auth_service
        RETURNING VALUE(ro_repo_auth_service) TYPE REF TO if_abapgit_repository_auth,
      get_branches
        IMPORTING iv_url                TYPE string
        RETURNING VALUE(rt_branches) TYPE if_abapgit_definitions=>ty_git_branch_list_tt
        RAISING   cx_abapgit_exception.
ENDCLASS.



CLASS cl_abapgit_res_repo_info_ext IMPLEMENTATION.


  METHOD post.

    DATA:
      ls_request_data  TYPE ty_request_data,
      ls_response_data TYPE ty_response_data.

    DATA(ls_requested_content_type) =
      request->get_inner_rest_request( )->get_header_field( iv_name = if_http_header_fields=>content_type ).

    CASE ls_requested_content_type.

      WHEN co_content_type_request_v1.
        " Request Content Handler
        " Wrap the request content handler in a cl_adt_rest_comp_cnt_handler in order to ensure that the client
        " sends a correct 'Content-Type:' header
        DATA(lo_request_content_handler) = cl_adt_rest_comp_cnt_handler=>create(
             request         = request
             content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                         st_name      = co_st_name_request
                         root_name    = co_root_name_request
                         content_type = co_content_type_request_v1 ) ).

        " Response Content Handler
        DATA(lo_response_content_handler) = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
             st_name      = co_st_name_response
             root_name    = co_root_name_response
             content_type = co_content_type_response_v1 ).

      WHEN co_content_type_request_v2.
        " Request Content Handler
        " Wrap the request content handler in a cl_adt_rest_comp_cnt_handler in order to ensure that the client
        " sends a correct 'Content-Type:' header
        lo_request_content_handler = cl_adt_rest_comp_cnt_handler=>create(
             request         = request
             content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                         st_name      = co_st_name_request_v2
                         root_name    = co_root_name_request
                         content_type = co_content_type_request_v2 ) ).

        " Response Content Handler
        lo_response_content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
             st_name      = co_st_name_response_v2
             root_name    = co_root_name_response
             content_type = co_content_type_response_v2 ).

      WHEN OTHERS.
        response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
    ENDCASE.


    " Validation of request 'Accept:' header
    cl_adt_rest_comp_cnt_handler=>create( request = request
                                          content_handler = lo_response_content_handler )->check_cnt_type_is_supported( ).

    " Retrieve request data
    request->get_body_data(
      EXPORTING
        content_handler = lo_request_content_handler
      IMPORTING
        data            = ls_request_data ).

    TRY.
        " check for user/password information in passed URL and filter it
        IF cl_abapgit_url=>has_credentials( |{ ls_request_data-url }| ).
          ls_request_data-url = cl_abapgit_url=>decomposed_url( ls_request_data-url ).
          IF ls_request_data-user IS INITIAL AND ls_request_data-password IS INITIAL.
            ls_request_data-user = cl_abapgit_url=>user( ls_request_data-url ).
            ls_request_data-password = cl_abapgit_url=>password( ls_request_data-url ).
          ENDIF.
        ENDIF.

        " Set logon information if supplied
        IF ls_request_data-user     IS NOT INITIAL AND
           ls_request_data-password IS NOT INITIAL.
          cl_abapgit_default_auth_info=>refresh( ).
          cl_abapgit_default_auth_info=>set_auth_info( iv_user     = ls_request_data-user
                                                       iv_password = ls_request_data-password ).
        ENDIF.

        " Check whether passed repo URL has public or privated access
        mo_repo_auth_srv = get_repo_auth_service( ).
        ls_response_data-access_mode = mo_repo_auth_srv->determine_access_level( ls_request_data-url ).

        " Check whether two factor authentication is enabled
        IF ls_response_data-access_mode <> 'PUBLIC' AND
           ls_request_data-user IS NOT INITIAL AND
           ls_request_data-password IS NOT INITIAL AND
           mo_repo_auth_srv->is_2fa_required( iv_url         = ls_request_data-url
                                              iv_username    = ls_request_data-user
                                              iv_password    = ls_request_data-password
                                              iv_trigger_sms = abap_false ) = abap_true.
          cx_abapgit_exception=>raise( '2FA required' ).
        ENDIF.


        " Retrieve list of branches for repo
        IF ls_response_data-access_mode = 'PUBLIC' OR
          ( ls_response_data-access_mode = 'PRIVATE' AND
            ls_request_data-user         IS NOT INITIAL AND
            ls_request_data-password     IS NOT INITIAL ).
          mt_branches = get_branches( ls_request_data-url ).
          APPEND LINES OF mt_branches TO ls_response_data-branches.
        ENDIF.

        " Prepare Response
        response->set_body_data( content_handler = lo_response_content_handler data = ls_response_data ).
        response->set_status( cl_rest_status_code=>gc_success_ok ).

      CATCH cx_abapgit_exception INTO DATA(lx_abapgit_exception).

        DATA lv_http_status TYPE i.
        " Check whether the exception occurred because of any authentication issues
        IF mo_repo_auth_srv->is_authorization_issue( EXPORTING  ix_abapgit_exception = lx_abapgit_exception
                                                     IMPORTING ev_http_status = lv_http_status
                                                      ).
          mo_repo_auth_srv->handle_auth_exception( iv_http_status = lv_http_status
                                                   ix_abapgit_exception = lx_abapgit_exception ).
        ELSE.
          cx_adt_rest_abapgit=>raise_with_error( ix_error = lx_abapgit_exception
                                                 iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.

  METHOD get_repo_auth_service.
    IF mo_repo_auth_srv IS INITIAL.
      ro_repo_auth_service = NEW cl_abapgit_repository_auth( ).
    ELSE.
      ro_repo_auth_service = mo_repo_auth_srv.
    ENDIF.
  ENDMETHOD.

  METHOD get_branches.
    IF mt_branches IS INITIAL.
      DATA(lo_branch_list) = cl_abapgit_git_transport=>branches( iv_url ).
      rt_branches = lo_branch_list->get_branches_only( ).
    ELSE.
      rt_branches = mt_branches.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
