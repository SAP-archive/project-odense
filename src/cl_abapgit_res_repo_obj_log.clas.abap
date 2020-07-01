CLASS cl_abapgit_res_repo_obj_log DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF t_obj_result,
        obj_type   TYPE trobjtype,
        obj_name   TYPE sobj_name,
        obj_status TYPE symsgty,
        package    TYPE devclass,
        msg_type   TYPE symsgty,
        msg_text   TYPE string,
      END OF t_obj_result,
      tt_obj_result TYPE STANDARD TABLE OF t_obj_result WITH DEFAULT KEY.

    CONSTANTS co_st_name_post_res       TYPE string VALUE 'ABAPGIT_ST_REPO_POST_RES'.
    CONSTANTS co_root_name_post_res     TYPE string VALUE 'OBJECTS'.
    CONSTANTS co_content_type_object_v1 TYPE string VALUE 'application/abapgit.adt.repo.object.v1+xml' ##NO_TEXT.
    CONSTANTS co_st_name_post_res_v2    TYPE string VALUE 'ABAPGIT_ST_REPO_POST_RES_V2'.
    CONSTANTS co_content_type_object_v2 TYPE string VALUE 'application/abapgit.adt.repo.object.v2+xml' ##NO_TEXT.

    METHODS get REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS cl_abapgit_res_repo_obj_log IMPLEMENTATION.


  METHOD get.

    DATA lv_app_log_key  TYPE tsa4c_agit_applog_key-app_log.
    DATA lt_result_table TYPE tt_obj_result.
    DATA ls_result_table TYPE t_obj_result.

    DATA(ls_requested_content_type) =
      request->get_inner_rest_request( )->get_header_field( iv_name = if_http_header_fields=>accept ).

    CASE ls_requested_content_type.

      WHEN co_content_type_object_v1.
        DATA(lo_resp_content_handler) = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                                        st_name      = co_st_name_post_res
                                        root_name    = co_root_name_post_res
                                        content_type = co_content_type_object_v1 ).

      WHEN co_content_type_object_v2.
        lo_resp_content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                                        st_name      = co_st_name_post_res_v2
                                        root_name    = co_root_name_post_res
                                        content_type = co_content_type_object_v2 ).
      WHEN OTHERS.
        response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).

    ENDCASE.


    " validation of request 'Accept:' header
    cl_adt_rest_comp_cnt_handler=>create( request = request
                                          content_handler = lo_resp_content_handler )->check_cnt_type_is_supported( ).

    TRY.

        " Get Application Log Key
        request->get_uri_attribute( EXPORTING name = 'app_log_key' mandatory = abap_true
                                    IMPORTING value = lv_app_log_key ).

        " read application log
        DATA(lo_log_factory) = cl_abapgit_app_log_factory=>get_instance( ).
        DATA(lo_log) = lo_log_factory->load_single( iv_app_log = lv_app_log_key ).

        " transform application log to result (object based)
        DATA lt_obj_result TYPE cl_abapgit_res_util=>tt_obj_result.
        cl_abapgit_res_util=>get_obj_result_from_log(
          EXPORTING iv_log        = lo_log
          IMPORTING et_obj_result = lt_obj_result ).

        response->set_body_data(
          content_handler = lo_resp_content_handler
          data            = lt_obj_result ).

        response->set_status( cl_rest_status_code=>gc_success_ok ).

      " Handle issues
      CATCH cx_abapgit_exception cx_abapgit_app_log INTO DATA(lx_exception).
        ROLLBACK WORK.
        cx_adt_rest_abapgit=>raise_with_error(
            ix_error       = lx_exception
            iv_http_status = cl_rest_status_code=>gc_server_error_internal ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
