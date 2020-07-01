CLASS ltcl_repository_checks DEFINITION DEFERRED.
CLASS cl_abapgit_res_repo_checks DEFINITION LOCAL FRIENDS ltcl_repository_checks.

CLASS ltcl_repository_checks DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    CONSTANTS:
      co_http_status    TYPE string VALUE 'http_status',
      co_test_user_name TYPE string VALUE 'test_user',
      co_test_pass      TYPE string VALUE 'dGVzdF9wYXNzd29yZA==',
      co_repo_key       TYPE string VALUE '12345'.

    CLASS-DATA:
      lo_repository_service TYPE REF TO if_abapgit_repo_srv,
      f_cut                 TYPE REF TO cl_abapgit_res_repo_checks,
      mo_repo_online        TYPE REF TO cl_abapgit_repo_online.

    CLASS-METHODS:
      class_setup RAISING cx_static_check,
      class_teardown.

    METHODS:
      authorization_issue_401_error  FOR TESTING RAISING cx_static_check,
      authorization_issue_403_error  FOR TESTING RAISING cx_static_check,
      authorization_issue_404_error  FOR TESTING RAISING cx_static_check,
      no_authorization_error         FOR TESTING RAISING cx_static_check,
      conn_issue_but_not_auth_issue  FOR TESTING RAISING cx_static_check,
      authorization_issue_2fa_error  FOR TESTING RAISING cx_static_check,

      get_abapgit_exception
        IMPORTING iv_exc_text                 TYPE string
        RETURNING VALUE(ro_abapgit_exception) TYPE REF TO cx_abapgit_exception,

      get_request_stub_4_conn_check
        CHANGING co_request_stub TYPE REF TO cl_adt_rest_request_stub.

ENDCLASS.

CLASS ltcl_repository_checks IMPLEMENTATION.

  METHOD class_setup.

    " create code under test instance
    CREATE OBJECT f_cut.

    " create test double for repository service
    lo_repository_service ?= cl_abap_testdouble=>create( 'if_abapgit_repo_srv' ).
    " configure repository service test double
    cl_abap_testdouble=>configure_call( lo_repository_service )->returning( mo_repo_online ).
    lo_repository_service->get( iv_key = '12345' ).

  ENDMETHOD.

  METHOD class_teardown.

  ENDMETHOD.

  METHOD authorization_issue_401_error.

    DATA: lo_repository_checks TYPE REF TO if_abapgit_repository_checks,
          lo_request_stub      TYPE REF TO cl_adt_rest_request_stub,
          lo_response_spy      TYPE REF TO cl_adt_rest_response_spy,
          lv_http_status       TYPE string.

    " create test double repository check service
    lo_repository_checks ?= cl_abap_testdouble=>create( 'if_abapgit_repository_checks' ).
    " configure repository check service  test double
    cl_abap_testdouble=>configure_call( lo_repository_checks )->raise_exception( get_abapgit_exception( 'HTTP 401, unauthorized' ) ).
    lo_repository_checks->check_connection( io_repository = mo_repo_online iv_service = 'receive' ).

    " create request stub for check POST call
    get_request_stub_4_conn_check( CHANGING co_request_stub = lo_request_stub ).

    " create response spy
    CREATE OBJECT lo_response_spy.

    "inject mocks
    f_cut->mo_repo_service = lo_repository_service.
    f_cut->mo_repo_check_service = lo_repository_checks.

    " test
    TRY.
        f_cut->post( request = lo_request_stub response = lo_response_spy ).
        cl_abap_unit_assert=>fail( 'Exception not raised' ).
      CATCH cx_adt_rest_abapgit INTO DATA(lx_abapgit_rest_exception).
        lx_abapgit_rest_exception->properties->get_property(
          EXPORTING
            key   = co_http_status
          IMPORTING
            value = lv_http_status
        ).
        cl_abap_unit_assert=>assert_equals( act = lv_http_status
                                            exp = cl_rest_status_code=>gc_client_error_unauthorized ).
    ENDTRY.

  ENDMETHOD.

  METHOD authorization_issue_403_error.

    DATA: lo_repository_checks TYPE REF TO if_abapgit_repository_checks,
          lo_request_stub      TYPE REF TO cl_adt_rest_request_stub,
          lo_response_spy      TYPE REF TO cl_adt_rest_response_spy,
          lv_http_status       TYPE string.

    " create test double repository check service
    lo_repository_checks ?= cl_abap_testdouble=>create( 'if_abapgit_repository_checks' ).
    " configure repository check service  test double
    cl_abap_testdouble=>configure_call( lo_repository_checks )->raise_exception( get_abapgit_exception( 'HTTP 403, forbidden' ) ).
    lo_repository_checks->check_connection( io_repository = mo_repo_online iv_service = 'receive' ).

    " create request stub for check POST call
    get_request_stub_4_conn_check( CHANGING co_request_stub = lo_request_stub ).

    " create response spy
    CREATE OBJECT lo_response_spy.

    " inject mocks
    f_cut->mo_repo_service = lo_repository_service.
    f_cut->mo_repo_check_service = lo_repository_checks.

    " test
    TRY.
        f_cut->post( request = lo_request_stub response = lo_response_spy ).
        cl_abap_unit_assert=>fail( 'Exception not raised' ).
      CATCH cx_adt_rest_abapgit INTO DATA(lx_abapgit_rest_exception).
        lx_abapgit_rest_exception->properties->get_property(
          EXPORTING
            key   = co_http_status
          IMPORTING
            value = lv_http_status
        ).
        cl_abap_unit_assert=>assert_equals( act = lv_http_status exp = cl_rest_status_code=>gc_client_error_forbidden ).
    ENDTRY.

  ENDMETHOD.

  METHOD authorization_issue_404_error.

    DATA: lo_repository_checks TYPE REF TO if_abapgit_repository_checks,
          lo_request_stub      TYPE REF TO cl_adt_rest_request_stub,
          lo_response_spy      TYPE REF TO cl_adt_rest_response_spy,
          lv_http_status       TYPE string.

    " create test double repository check service
    lo_repository_checks ?= cl_abap_testdouble=>create( 'if_abapgit_repository_checks' ).
    " configure repository check service  test double
    cl_abap_testdouble=>configure_call( lo_repository_checks )->raise_exception( get_abapgit_exception( 'HTTP 404, not found' ) ).
    lo_repository_checks->check_connection( io_repository = mo_repo_online iv_service = 'receive' ).

    " create request stub for check POST call
    get_request_stub_4_conn_check( CHANGING co_request_stub = lo_request_stub ).

    " create response spy
    CREATE OBJECT lo_response_spy.

    " inject mocks
    f_cut->mo_repo_service = lo_repository_service.
    f_cut->mo_repo_check_service = lo_repository_checks.

    " test
    TRY.
        f_cut->post( request = lo_request_stub response = lo_response_spy ).
        cl_abap_unit_assert=>fail( 'Exception not raised' ).
      CATCH cx_adt_rest_abapgit INTO DATA(lx_abapgit_rest_exception).
        lx_abapgit_rest_exception->properties->get_property(
          EXPORTING
            key   = co_http_status
          IMPORTING
            value = lv_http_status
        ).
        cl_abap_unit_assert=>assert_equals( act = lv_http_status exp = cl_rest_status_code=>gc_client_error_not_found ).
    ENDTRY.

  ENDMETHOD.

  METHOD conn_issue_but_not_auth_issue.

    DATA: lo_repository_checks TYPE REF TO if_abapgit_repository_checks,
          lo_request_stub      TYPE REF TO cl_adt_rest_request_stub,
          lo_response_spy      TYPE REF TO cl_adt_rest_response_spy,
          lv_http_status       TYPE string.

    " create test double repository check service
    lo_repository_checks ?= cl_abap_testdouble=>create( 'if_abapgit_repository_checks' ).
    " configure repository check service  test double
    cl_abap_testdouble=>configure_call( lo_repository_checks )->raise_exception( get_abapgit_exception( 'HTTP redirect, check URL' ) ).
    lo_repository_checks->check_connection( io_repository = mo_repo_online iv_service = 'receive' ).

    " create request stub for check POST call
    get_request_stub_4_conn_check( CHANGING co_request_stub = lo_request_stub ).

    " create response spy
    CREATE OBJECT lo_response_spy.

    " inject mocks
    f_cut->mo_repo_service = lo_repository_service.
    f_cut->mo_repo_check_service = lo_repository_checks.

    " test
    TRY.
        f_cut->post( request = lo_request_stub response = lo_response_spy ).
        cl_abap_unit_assert=>fail( 'Exception not raised' ).
      CATCH cx_adt_rest_abapgit INTO DATA(lx_abapgit_rest_exception).
        "expected
    ENDTRY.

  ENDMETHOD.

  METHOD no_authorization_error.

    DATA: lo_repository_checks TYPE REF TO if_abapgit_repository_checks,
          lo_request_stub      TYPE REF TO cl_adt_rest_request_stub,
          lo_response_spy      TYPE REF TO cl_adt_rest_response_spy,
          lv_http_status       TYPE string.

    " create test double repository check service
    lo_repository_checks ?= cl_abap_testdouble=>create( 'if_abapgit_repository_checks' ).

    " create request stub for check POST call
    get_request_stub_4_conn_check( CHANGING co_request_stub = lo_request_stub ).

    " create response spy
    CREATE OBJECT lo_response_spy.

    " inject mocks
    f_cut->mo_repo_service = lo_repository_service.
    f_cut->mo_repo_check_service = lo_repository_checks.

    " test
    TRY.
        f_cut->post( request = lo_request_stub response = lo_response_spy ).
        cl_abap_unit_assert=>assert_equals( act =  lo_response_spy->get_status( )
                                            exp = cl_rest_status_code=>gc_success_ok ).
      CATCH cx_adt_rest_abapgit INTO DATA(lx_abapgit_rest_exception).
        cl_abap_unit_assert=>fail( 'No exception expected' ).
    ENDTRY.

  ENDMETHOD.

  METHOD authorization_issue_2fa_error.

    DATA: lo_repository_checks TYPE REF TO if_abapgit_repository_checks,
          lo_request_stub      TYPE REF TO cl_adt_rest_request_stub,
          lo_response_spy      TYPE REF TO cl_adt_rest_response_spy,
          lv_http_status       TYPE string.

    " create test double repository check service
    lo_repository_checks ?= cl_abap_testdouble=>create( 'if_abapgit_repository_checks' ).
    " configure repository check service  test double
    cl_abap_testdouble=>configure_call( lo_repository_checks )->raise_exception( get_abapgit_exception( '2FA required' ) ).
    lo_repository_checks->check_connection( io_repository = mo_repo_online iv_service = 'receive' ).

    " create request stub for check POST call
    get_request_stub_4_conn_check( CHANGING co_request_stub = lo_request_stub ).

    " create response spy
    CREATE OBJECT lo_response_spy.

    " inject mocks
    f_cut->mo_repo_service = lo_repository_service.
    f_cut->mo_repo_check_service = lo_repository_checks.

    " test
    TRY.
        f_cut->post( request = lo_request_stub response = lo_response_spy ).
        cl_abap_unit_assert=>fail( 'Exception not raised' ).
      CATCH cx_adt_rest_abapgit INTO DATA(lx_abapgit_rest_exception).
        lx_abapgit_rest_exception->properties->get_property(
          EXPORTING
            key   = co_http_status
          IMPORTING
            value = lv_http_status
        ).
        cl_abap_unit_assert=>assert_equals( act = lv_http_status
                                            exp = cl_rest_status_code=>gc_client_error_unauthorized ).

        DATA short_text TYPE string.
        MESSAGE e008(a4c_agit_adt) INTO short_text.
        cl_abap_unit_assert=>assert_equals( act = lx_abapgit_rest_exception->get_text( )
                                            exp = short_text ).

    ENDTRY.

  ENDMETHOD.

  METHOD get_abapgit_exception.
    TRY.
        cx_abapgit_exception=>raise( iv_exc_text ).
      CATCH cx_abapgit_exception INTO DATA(lx_abapgit_exception).
        ro_abapgit_exception = lx_abapgit_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD get_request_stub_4_conn_check.
    CREATE OBJECT co_request_stub EXPORTING uri = '/sap/bc/adt/abapgit/repos/12345/checks'.
    co_request_stub->set_uri_attribute( name = 'key' value = co_repo_key ).
    co_request_stub->add_header_field( key = 'Username' value = co_test_user_name ).
    co_request_stub->add_header_field( key = 'Password' value = co_test_pass ).
  ENDMETHOD.


ENDCLASS.
