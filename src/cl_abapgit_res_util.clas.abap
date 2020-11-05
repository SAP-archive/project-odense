CLASS cl_abapgit_res_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

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

    CLASS-METHODS:
      get_obj_result_from_log
        IMPORTING
          iv_log        TYPE REF TO if_abapgit_log
        RETURNING VALUE(rt_obj_result) TYPE tt_obj_result.

    CLASS-METHODS:
      encode_password
        IMPORTING iv_password        TYPE string
        RETURNING VALUE(rv_password) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS cl_abapgit_res_util IMPLEMENTATION.

  METHOD get_obj_result_from_log.

    DATA ls_obj_result TYPE t_obj_result.
    CLEAR rt_obj_result.
    CHECK iv_log IS BOUND.

    iv_log->get_item_status( IMPORTING et_item_status = DATA(lt_item_status) ).

    LOOP AT lt_item_status REFERENCE INTO DATA(lr_item_status).

      CLEAR ls_obj_result.
      ls_obj_result-obj_name   = lr_item_status->item-obj_name.
      ls_obj_result-obj_type   = lr_item_status->item-obj_type.
      ls_obj_result-package    = lr_item_status->item-devclass.
      ls_obj_result-obj_status = lr_item_status->status.

      DATA(lt_msg) = lr_item_status->messages.
      LOOP AT lt_msg ASSIGNING FIELD-SYMBOL(<ls_msg>).
        ls_obj_result-msg_type = <ls_msg>-type.
        ls_obj_result-msg_text = <ls_msg>-text.
        APPEND ls_obj_result TO rt_obj_result.
      ENDLOOP.
      IF sy-subrc <> 0.
        APPEND ls_obj_result TO rt_obj_result.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD encode_password.
    rv_password = cl_http_utility=>decode_base64( iv_password ).
  ENDMETHOD.

ENDCLASS.
