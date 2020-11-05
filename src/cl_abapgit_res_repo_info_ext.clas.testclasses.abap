*"* use this source file for your ABAP unit test classes

CLASS ltcl_simple_transformation DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:
      pos_st_i_ext_req_seq_ok     FOR TESTING RAISING cx_static_check,
      pos_st_i_ext_req_seq_not_ok FOR TESTING RAISING cx_static_check,
      pos_st_i_ext_req_new_field  FOR TESTING RAISING cx_static_check,
      pos_st_i_ext_req_field_miss FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_simple_transformation IMPLEMENTATION.

  METHOD pos_st_i_ext_req_seq_ok.
    " Test: Sequence of fields in input XML is equal to fields operated by simple transformation
    " Result: Transformation should succeed

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE cl_abapgit_res_repo_info_ext=>ty_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.

    DATA lv_xml TYPE string.
    lv_xml = lv_xml && |<?xml version="1.0" encoding="utf-8"?>|.
    lv_xml = lv_xml && |<repository_ext>|.
    lv_xml = lv_xml && |  <url>a</url>|.
    lv_xml = lv_xml && |  <user>b</user>|.
    lv_xml = lv_xml && |  <password>c</password>|.
    lv_xml = lv_xml && |</repository_ext>|.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = cl_abapgit_res_repo_info_ext=>co_root_name_request.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_info_ext_req
      SOURCE XML lv_input_xml
      RESULT (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-url
                                        exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user
                                        exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password
                                        exp = 'c' ).

  ENDMETHOD.

  METHOD pos_st_i_ext_req_seq_not_ok.
    " Test: Sequence of fields in input XML is NOT equal to fields operated by simple transformation
    " Result: Transformation should succeed

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE cl_abapgit_res_repo_info_ext=>ty_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.

    DATA lv_xml TYPE string.
    lv_xml = lv_xml && |<?xml version="1.0" encoding="utf-8"?>|.
    lv_xml = lv_xml && |<repository_ext>|.
    lv_xml = lv_xml && |  <url>a</url>|.
    lv_xml = lv_xml && |  <password>c</password>|. "exchanged by USER
    lv_xml = lv_xml && |  <user>b</user>|. "exchanged by PASSWORD
    lv_xml = lv_xml && |</repository_ext>|.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = cl_abapgit_res_repo_info_ext=>co_root_name_request.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_info_ext_req
      SOURCE XML lv_input_xml
      RESULT (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-url
                                        exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user
                                        exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password
                                        exp = 'c' ).

  ENDMETHOD.

  METHOD pos_st_i_ext_req_new_field.
    " Test: Input XML contains a field that is not part of ABAP structure
    " Result: Transformation should succeed, new field should be ignored

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE cl_abapgit_res_repo_info_ext=>ty_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.

    DATA lv_xml TYPE string.
    lv_xml = lv_xml && |<?xml version="1.0" encoding="utf-8"?>|.
    lv_xml = lv_xml && |<repository_ext>|.
    lv_xml = lv_xml && |  <url>a</url>|.
    lv_xml = lv_xml && |  <dontexist>x</dontexist>|. "field don't exist in structure
    lv_xml = lv_xml && |  <user>b</user>|.
    lv_xml = lv_xml && |  <password>c</password>|.
    lv_xml = lv_xml && |</repository_ext>|.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = cl_abapgit_res_repo_info_ext=>co_root_name_request.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_info_ext_req
      SOURCE XML lv_input_xml
      RESULT (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-url
                                        exp = 'a' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-user
                                        exp = 'b' ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-password
                                        exp = 'c' ).

  ENDMETHOD.

  METHOD pos_st_i_ext_req_field_miss.
    " Test: In input XML a field defined in ABAP structure is missing
    " Result: Transformation should succeed, missing field should be initial

    DATA lv_input_xml TYPE xstring.
    DATA lt_result    TYPE abap_trans_resbind_tab.
    DATA ls_data      TYPE cl_abapgit_res_repo_info_ext=>ty_request_data.
    FIELD-SYMBOLS <lt_result> LIKE LINE OF lt_result.

    DATA lv_xml TYPE string.
    lv_xml = lv_xml && |<?xml version="1.0" encoding="utf-8"?>|.
    lv_xml = lv_xml && |<repository_ext>|.
    lv_xml = lv_xml && |  <url>a</url>|.
    "lv_xml = lv_xml && |  <user>b</user>|.  "field missing()
    lv_xml = lv_xml && |  <password>c</password>|.
    lv_xml = lv_xml && |</repository_ext>|.
    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_input_xml.

    APPEND INITIAL LINE TO lt_result ASSIGNING <lt_result>.
    <lt_result>-name = cl_abapgit_res_repo_info_ext=>co_root_name_request.
    GET REFERENCE OF ls_data INTO <lt_result>-value.

    CALL TRANSFORMATION abapgit_st_repo_info_ext_req
      SOURCE XML lv_input_xml
      RESULT (lt_result).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lt_result ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-url
                                        exp = 'a' ).
    cl_abap_unit_assert=>assert_initial( act = ls_data-user ). "initial value expected
    cl_abap_unit_assert=>assert_equals( act = ls_data-password
                                        exp = 'c' ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_simple_transformation_v2 DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:
      pos_st_i_ext_req_seq_ok       FOR TESTING RAISING cx_static_check,
      pos_st_i_ext_req_seq_not_ok   FOR TESTING RAISING cx_static_check,
      pos_st_i_ext_req_new_field    FOR TESTING RAISING cx_static_check,
      pos_st_i_ext_req_field_miss   FOR TESTING RAISING cx_static_check,
      pos_st_i_ext_res_serialize_ok FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_simple_transformation_v2 IMPLEMENTATION.

  METHOD pos_st_i_ext_req_field_miss.

    " Test: Input XML has a missing field from fields in ABAP Structure operated by ST
    " Result: Transformation should work, missing field is initial

    DATA lv_request_data TYPE cl_abapgit_res_repo_info_ext=>ty_request_data.

    DATA lv_xml TYPE string.
    lv_xml = lv_xml && |<?xml version="1.0" encoding="utf-16"?>|.
    lv_xml = lv_xml && |<abapgitexternalrepo:externalRepoInfoRequest xmlns:abapgitexternalrepo="http://www.sap.com/adt/abapgit/externalRepo">|.
    lv_xml = lv_xml && | <abapgitexternalrepo:url>dummy_url</abapgitexternalrepo:url>|.
    "lv_xml = lv_xml && | <abapgitexternalrepo:user>dummy_user</abapgitexternalrepo:user>'.    missing field
    lv_xml = lv_xml && | <abapgitexternalrepo:password>dummy_password</abapgitexternalrepo:password>|.
    lv_xml = lv_xml && |</abapgitexternalrepo:externalRepoInfoRequest>|.

    CALL TRANSFORMATION abapgit_st_repo_info_ext_rq_v2
      SOURCE XML lv_xml
      RESULT repository_external_req = lv_request_data.

    cl_abap_unit_assert=>assert_equals( exp = 'dummy_url'
                                        act = lv_request_data-url ).
    cl_abap_unit_assert=>assert_initial( lv_request_data-user ).
    cl_abap_unit_assert=>assert_equals( exp = 'dummy_password'
                                        act = lv_request_data-password ).

  ENDMETHOD.

  METHOD pos_st_i_ext_req_new_field.

    " Test: Input XML has a new field which is not in the fields in ABAP Structure operated by ST
    " Result: Transformation should work

    DATA lv_request_data TYPE cl_abapgit_res_repo_info_ext=>ty_request_data.

    DATA lv_xml TYPE string.
    lv_xml = lv_xml && |<?xml version="1.0" encoding="utf-16"?>|.
    lv_xml = lv_xml && |<abapgitexternalrepo:externalRepoInfoRequest xmlns:abapgitexternalrepo="http://www.sap.com/adt/abapgit/externalRepo">|.
    lv_xml = lv_xml && | <abapgitexternalrepo:user>dummy_user</abapgitexternalrepo:user>|. "exchanged by url
    lv_xml = lv_xml && | <abapgitexternalrepo:url>dummy_url</abapgitexternalrepo:url>|.  "exchanged by user
    lv_xml = lv_xml && | <abapgitexternalrepo:password>dummy_password</abapgitexternalrepo:password>|.
    lv_xml = lv_xml && | <abapgitexternalrepo:newField>dummy_field</abapgitexternalrepo:newField>|.
    lv_xml = lv_xml && |</abapgitexternalrepo:externalRepoInfoRequest>|.

    CALL TRANSFORMATION abapgit_st_repo_info_ext_rq_v2
      SOURCE XML lv_xml
      RESULT repository_external_req = lv_request_data.

    cl_abap_unit_assert=>assert_equals( exp = 'dummy_url'
                                        act = lv_request_data-url ).
    cl_abap_unit_assert=>assert_equals( exp = 'dummy_user'
                                        act = lv_request_data-user ).
    cl_abap_unit_assert=>assert_equals( exp = 'dummy_password'
                                        act = lv_request_data-password ).



  ENDMETHOD.

  METHOD pos_st_i_ext_req_seq_not_ok.
    " Test: Sequence of fields in Input XML is not same as the sequence of fields in Abap Structure operated by ST
    " Result: Transformation should work

    DATA lv_request_data TYPE cl_abapgit_res_repo_info_ext=>ty_request_data.

    DATA lv_xml TYPE string.
    lv_xml = lv_xml && |<?xml version="1.0" encoding="utf-16"?>|.
    lv_xml = lv_xml && |<abapgitexternalrepo:externalRepoInfoRequest xmlns:abapgitexternalrepo="http://www.sap.com/adt/abapgit/externalRepo">|.
    lv_xml = lv_xml && | <abapgitexternalrepo:user>dummy_user</abapgitexternalrepo:user>|. "exchanged by url
    lv_xml = lv_xml && | <abapgitexternalrepo:url>dummy_url</abapgitexternalrepo:url>|. "exchanged by user
    lv_xml = lv_xml && | <abapgitexternalrepo:password>dummy_password</abapgitexternalrepo:password>|.
    lv_xml = lv_xml && |</abapgitexternalrepo:externalRepoInfoRequest>|.

    CALL TRANSFORMATION abapgit_st_repo_info_ext_rq_v2
      SOURCE XML lv_xml
      RESULT repository_external_req = lv_request_data.

    cl_abap_unit_assert=>assert_equals( exp = 'dummy_url'
                                        act = lv_request_data-url ).
    cl_abap_unit_assert=>assert_equals( exp = 'dummy_user'
                                        act = lv_request_data-user ).
    cl_abap_unit_assert=>assert_equals( exp = 'dummy_password'
                                        act = lv_request_data-password ).

  ENDMETHOD.

  METHOD pos_st_i_ext_req_seq_ok.
    " Test: Sequence of fields in Input XMl is same as the sequence of fields in Abap Structure operated by ST
    " Result: Transformation should work

    DATA ls_request_data TYPE cl_abapgit_res_repo_info_ext=>ty_request_data.

    DATA lv_xml TYPE string.
    lv_xml = lv_xml && |<?xml version="1.0" encoding="utf-16"?>|.
    lv_xml = lv_xml && |<abapgitexternalrepo:externalRepoInfoRequest xmlns:abapgitexternalrepo="http://www.sap.com/adt/abapgit/externalRepo">|.
    lv_xml = lv_xml && | <abapgitexternalrepo:url>dummy_url</abapgitexternalrepo:url>|.
    lv_xml = lv_xml && | <abapgitexternalrepo:user>dummy_user</abapgitexternalrepo:user>|.
    lv_xml = lv_xml && | <abapgitexternalrepo:password>dummy_password</abapgitexternalrepo:password>|.
    lv_xml = lv_xml && |</abapgitexternalrepo:externalRepoInfoRequest>|.

    CALL TRANSFORMATION abapgit_st_repo_info_ext_rq_v2
      SOURCE XML lv_xml
      RESULT repository_external_req = ls_request_data.

    cl_abap_unit_assert=>assert_equals( exp = 'dummy_url'
                                        act = ls_request_data-url ).
    cl_abap_unit_assert=>assert_equals( exp = 'dummy_user'
                                        act = ls_request_data-user ).
    cl_abap_unit_assert=>assert_equals( exp = 'dummy_password'
                                        act = ls_request_data-password ).

  ENDMETHOD.

  METHOD pos_st_i_ext_res_serialize_ok.
    "Test: Serialize response_data using ST
    "Result: Output XML by ST should be equal expected XML

    DATA lv_xml TYPE string.
    lv_xml = lv_xml && |<?xml version="1.0" encoding="utf-16"?>|.
    lv_xml = lv_xml && |<abapgitexternalrepo:externalRepoInfo xmlns:abapgitexternalrepo="http://www.sap.com/adt/abapgit/externalRepo">|.
    lv_xml = lv_xml && |<abapgitexternalrepo:accessMode>public</abapgitexternalrepo:accessMode>|.
    lv_xml = lv_xml && |<abapgitexternalrepo:branch>|.
    lv_xml = lv_xml && |<abapgitexternalrepo:sha1>dummy_sha</abapgitexternalrepo:sha1>|.
    lv_xml = lv_xml && |<abapgitexternalrepo:name>dummy_name</abapgitexternalrepo:name>|.
    lv_xml = lv_xml && |<abapgitexternalrepo:type>t1</abapgitexternalrepo:type>|.
    lv_xml = lv_xml && |<abapgitexternalrepo:isHead>X</abapgitexternalrepo:isHead>|.
    lv_xml = lv_xml && |<abapgitexternalrepo:displayName>dummy_display_name</abapgitexternalrepo:displayName>|.
    lv_xml = lv_xml && |</abapgitexternalrepo:branch>|.
    lv_xml = lv_xml && |</abapgitexternalrepo:externalRepoInfo>|.


    DATA ls_response_data TYPE cl_abapgit_res_repo_info_ext=>ty_response_data.
    ls_response_data-access_mode ='public'.
    ls_response_data-branches = VALUE #( ( sha1         = 'dummy_sha'
                                           name         = 'dummy_name'
                                           type         = 't1'
                                           is_head      = abap_true
                                           display_name = 'dummy_display_name' ) ).

    DATA lv_xml_output TYPE string.
    CALL TRANSFORMATION abapgit_st_repo_info_ext_rs_v2
      SOURCE repository_external = ls_response_data
      RESULT XML lv_xml_output.

    CALL TRANSFORMATION id SOURCE XML lv_xml RESULT XML lv_xml.
    CALL TRANSFORMATION id SOURCE XML lv_xml_output RESULT XML lv_xml_output.


    cl_abap_unit_assert=>assert_equals( exp = lv_xml
                                        act = lv_xml_output ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_post_method DEFINITION FINAL FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.

    CONSTANTS:
      co_test_user_name TYPE string VALUE 'test_user',
      co_test_pass      TYPE string VALUE 'dGVzdF9wYXNzd29yZA==',
      co_test_url       TYPE string VALUE 'http://githost.com/git_repo.git'.

    CLASS-DATA:
      mo_request_stub TYPE REF TO cl_adt_rest_request_stub,
      mo_response_spy TYPE REF TO cl_adt_rest_response_spy,
      mo_f_cut        TYPE REF TO cl_abapgit_res_repo_info_ext.

    CLASS-METHODS:
      class_setup RAISING cx_static_check.

    METHODS:
      test_post_public_repo_ok  FOR TESTING RAISING cx_static_check,
      test_post_private_repo_ok FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS cl_abapgit_res_repo_info_ext DEFINITION LOCAL FRIENDS ltcl_post_method.

CLASS ltcl_post_method IMPLEMENTATION.

  METHOD class_setup.

    mo_f_cut = NEW #( ).
    mo_request_stub = NEW #( ).
    mo_response_spy = NEW #( ).

  ENDMETHOD.

  METHOD test_post_public_repo_ok.

    "prepare request
    mo_request_stub->add_header_field( key   = if_http_header_fields=>content_type
                                       value = cl_abapgit_res_repo_info_ext=>co_content_type_request_v2 ).
    mo_request_stub->add_header_field( key   = if_http_header_fields=>accept
                                       value = cl_abapgit_res_repo_info_ext=>co_content_type_response_v2 ).

    DATA ls_request_data TYPE cl_abapgit_res_repo_info_ext=>ty_request_data.

    ls_request_data-url = co_test_url.

    mo_request_stub->set_body_data( data = ls_request_data ).

    "prepare test double
    DATA lv_abapgit_repo_auth_helper TYPE REF TO if_abapgit_repository_auth.
    lv_abapgit_repo_auth_helper ?= cl_abap_testdouble=>create( 'if_abapgit_repository_auth' ).

    "configure call
    cl_abap_testdouble=>configure_call( lv_abapgit_repo_auth_helper )->returning( 'PUBLIC' ).
    "which call
    lv_abapgit_repo_auth_helper->determine_access_level( co_test_url ).

    "Prepare test double
    DATA lt_branches TYPE if_abapgit_definitions=>ty_git_branch_list_tt.
    lt_branches = VALUE #( ( sha1 = 'a3e07c04158b8e2776d28e4185b5c9b128a9c330'
                             name = 'refs/heads/master'
                             type = 'HD'
                             is_head = abap_true
                             display_name = 'master' ) ).

    "inject test double
    mo_f_cut->mo_repo_auth_srv = lv_abapgit_repo_auth_helper.
    mo_f_cut->mt_branches = lt_branches.

    "call method under test
    mo_f_cut->post( request  = mo_request_stub
                    response = mo_response_spy ).

    cl_abap_unit_assert=>assert_equals( exp = cl_rest_status_code=>gc_success_ok
                                        act = mo_response_spy->get_status( ) ).

    DATA ls_response_data TYPE cl_abapgit_res_repo_info_ext=>ty_response_data.
    mo_response_spy->get_body_data( IMPORTING data = ls_response_data ).

    cl_abap_unit_assert=>assert_equals( act = ls_response_data-branches
                                        exp = lt_branches ).

  ENDMETHOD.

  METHOD test_post_private_repo_ok.

    "prepare request
    mo_request_stub->add_header_field( key   = if_http_header_fields=>content_type
                                       value = cl_abapgit_res_repo_info_ext=>co_content_type_request_v2 ).
    mo_request_stub->add_header_field( key   = if_http_header_fields=>accept
                                       value = cl_abapgit_res_repo_info_ext=>co_content_type_response_v2 ).

    DATA ls_request_data TYPE cl_abapgit_res_repo_info_ext=>ty_request_data.

    ls_request_data-url = co_test_url.
    ls_request_data-user = co_test_user_name.
    ls_request_data-password = co_test_pass.

    mo_request_stub->set_body_data( data = ls_request_data ).

    " prepare test double
    DATA lv_abapgit_repo_auth_helper TYPE REF TO if_abapgit_repository_auth.
    lv_abapgit_repo_auth_helper ?= cl_abap_testdouble=>create( 'if_abapgit_repository_auth' ).

    " configure call
    cl_abap_testdouble=>configure_call( lv_abapgit_repo_auth_helper )->returning( 'PRIVATE' ).
    " which call
    lv_abapgit_repo_auth_helper->determine_access_level( co_test_url ).

    " prepare test double
    DATA lv_authenticator TYPE REF TO if_abapgit_2fa_authenticator.
    lv_authenticator ?= cl_abap_testdouble=>create( 'if_abapgit_2fa_authenticator' ).

    " configure call
    cl_abap_testdouble=>configure_call( lv_authenticator )->returning( abap_false ).
    " which call
    lv_authenticator->is_2fa_required( iv_url         = co_test_url
                                       iv_username    = co_test_user_name
                                       iv_password    = co_test_pass
                                       iv_trigger_sms = abap_false ).


    " Prepare test double
    DATA lt_branches TYPE if_abapgit_definitions=>ty_git_branch_list_tt.
    lt_branches = VALUE #( ( sha1 = 'a3e07c04158b8e2776d28e4185b5c9b128a9c330'
                             name = 'refs/heads/master'
                             type = 'HD'
                             is_head = abap_true
                             display_name = 'master' ) ).

    " inject test double
    mo_f_cut->mo_repo_auth_srv = lv_abapgit_repo_auth_helper.
    mo_f_cut->mt_branches = lt_branches.

    " call method under test
    mo_f_cut->post( request  = mo_request_stub
                    response = mo_response_spy ).

    cl_abap_unit_assert=>assert_equals( exp = cl_rest_status_code=>gc_success_ok
                                        act = mo_response_spy->get_status( ) ).

    DATA ls_response_data TYPE cl_abapgit_res_repo_info_ext=>ty_response_data.
    mo_response_spy->get_body_data( IMPORTING data = ls_response_data ).

    cl_abap_unit_assert=>assert_equals( act = ls_response_data-branches
                                        exp = lt_branches ).

  ENDMETHOD.

ENDCLASS.
