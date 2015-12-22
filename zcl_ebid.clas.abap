CLASS zcl_ebid DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_destination TYPE pficf_destination_name DEFAULT 'EBID'
      RAISING
        zcx_ebid.
    METHODS test_connection
      RETURNING VALUE(rv_ok) TYPE boolean.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA http_client TYPE REF TO if_http_client.
    DATA rest_client TYPE REF TO cl_rest_http_client.
    DATA msg TYPE string.
ENDCLASS.



CLASS ZCL_EBID IMPLEMENTATION.


  METHOD constructor.
    cl_http_client=>create_by_destination(
      EXPORTING
        destination              = iv_destination    " Logical destination (specified in function call)
      IMPORTING
        client                   = me->http_client    " HTTP Client Abstraction
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 INTO msg.
      RAISE EXCEPTION TYPE zcx_ebid
        EXPORTING
          textid         = zcx_ebid=>generic_error
          exception_text = msg.
    ENDIF.

    CREATE OBJECT me->rest_client
      EXPORTING
        io_http_client = me->http_client.    " HTTP Client Object
  ENDMETHOD.


  METHOD test_connection.

    CONSTANTS: lc_test_path TYPE string VALUE '/ws/match/rest/v1.0/authorization-test'.

    me->rest_client->if_rest_client~set_request_header(
      EXPORTING
        iv_name  = if_http_header_fields_sap=>request_uri
        iv_value = lc_test_path
    ).
    me->rest_client->if_rest_client~set_request_header(
      EXPORTING
        iv_name  = if_http_header_fields=>content_type
        iv_value = 'application/json; charset=utf-8'
    ).
    me->rest_client->if_rest_client~set_request_header(
      EXPORTING
        iv_name  = if_http_header_fields=>accept
        iv_value = 'application/json'
    ).
    me->rest_client->if_rest_client~set_request_header(
      EXPORTING
        iv_name  = if_http_header_fields=>accept_encoding
        iv_value = 'charset=utf-8'
    ).

    me->rest_client->if_rest_client~get( ).
    DATA(lv_status) = me->rest_client->if_rest_client~get_status( ).
    IF lv_status = '200'.
      rv_ok = abap_true.
    ELSE.
      rv_ok = abap_false.
    ENDIF.
  ENDMETHOD.
ENDCLASS.