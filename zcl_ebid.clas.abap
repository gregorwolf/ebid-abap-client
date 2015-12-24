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
    METHODS match
      IMPORTING
        is_match_request         TYPE zebid_match_request
      RETURNING
        VALUE(rt_match_response) TYPE zebid_match_response_t
      RAISING
        zcx_ebid.
    METHODS get_company
      IMPORTING
        iv_ebid                    TYPE string
      RETURNING
        VALUE(rs_company_response) TYPE zebid_company_response
      RAISING
        zcx_ebid.
    CLASS-METHODS get_gguid
      RETURNING
        VALUE(rv_gguid) TYPE suid_uuid.
    class-METHODS copyright.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA http_client TYPE REF TO if_http_client.
    DATA rest_client TYPE REF TO cl_rest_http_client.
    DATA msg TYPE string.

    METHODS set_headers.
    METHODS get.
    METHODS post
      IMPORTING
        iv_data type string.
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


  METHOD copyright.

*--------------------------------------------------------------------*
*
* EBID ABAP client
* Copyright (C) 2015 Gregor Wolf
*
* Project home: https://github.com/gregorwolf/ebid-abap-client
*
* Published under Apache License, Version 2.0
* http://www.apache.org/licenses/LICENSE-2.0.html
*
*--------------------------------------------------------------------*

  ENDMETHOD.


  METHOD get.
    me->set_headers( ).
    me->rest_client->if_rest_client~get( ).
  ENDMETHOD.


  METHOD get_company.

    CONSTANTS: lc_company_path TYPE string VALUE '/ws/company/rest/v1.0/'.

    DATA: lv_path  TYPE string,
          ls_error TYPE zebid_error.

    lv_path = lc_company_path && iv_ebid.

    me->rest_client->if_rest_client~set_request_header(
      EXPORTING
        iv_name  = if_http_header_fields_sap=>request_uri
        iv_value = lv_path
    ).

    me->get( ).
    DATA(lv_status) = me->rest_client->if_rest_client~get_status( ).
    DATA(lo_entity) = me->rest_client->if_rest_client~get_response_entity( ).
    DATA(lv_json_res) = lo_entity->get_string_data( ).

    IF lv_status <> 200.
      /ui2/cl_json=>deserialize(
        EXPORTING
          json        = lv_json_res    " JSON string
          pretty_name = abap_true    " Pretty Print property names
        CHANGING
          data        = ls_error    " Data to serialize
      ).
      RAISE EXCEPTION TYPE zcx_ebid
        EXPORTING
          textid         = zcx_ebid=>generic_error
          exception_text = ls_error-errormessage.
    ENDIF.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = lv_json_res    " JSON string
        pretty_name = abap_true    " Pretty Print property names
      CHANGING
        data        = rs_company_response    " Data to serialize
    ).

  ENDMETHOD.


  METHOD get_gguid.
    DATA: lv_guid     TYPE guid_32.

    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_32 = lv_guid.    " Guid of length 32 (CHAR Format) Uppper Case

    rv_gguid = lv_guid.
    TRANSLATE rv_gguid TO LOWER CASE.
    CONCATENATE
        rv_gguid(8)
        rv_gguid+8(4)
        rv_gguid+12(4)
        rv_gguid+16(4)
        rv_gguid+20(12)
        INTO rv_gguid SEPARATED BY '-'.
  ENDMETHOD.


  METHOD match.
    DATA: ls_match_req TYPE zebid_match_request,
          ls_error     TYPE zebid_error.
    CONSTANTS: lc_match_path TYPE string VALUE '/ws/match/rest/v1.0/'.

    ls_match_req = is_match_request.
    ls_match_req-gguid = zcl_ebid=>get_gguid( ).

    DATA(lv_json) = /ui2/cl_json=>serialize(
      EXPORTING
        data        = ls_match_req
        compress    = abap_false
        pretty_name = abap_true
    ).

    me->rest_client->if_rest_client~set_request_header(
      EXPORTING
        iv_name  = if_http_header_fields_sap=>request_uri
        iv_value = lc_match_path
    ).

    me->post( lv_json ).
    DATA(lv_status) = me->rest_client->if_rest_client~get_status( ).
    DATA(lo_entity) = me->rest_client->if_rest_client~get_response_entity( ).
    DATA(lv_json_res) = lo_entity->get_string_data( ).

    IF lv_status <> 200.
      /ui2/cl_json=>deserialize(
        EXPORTING
          json        = lv_json_res    " JSON string
          pretty_name = abap_true    " Pretty Print property names
        CHANGING
          data        = ls_error    " Data to serialize
      ).
      RAISE EXCEPTION TYPE zcx_ebid
        EXPORTING
          textid         = zcx_ebid=>generic_error
          exception_text = ls_error-errormessage.
    ENDIF.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = lv_json_res    " JSON string
        pretty_name = abap_true    " Pretty Print property names
      CHANGING
        data        = rt_match_response    " Data to serialize
    ).

  ENDMETHOD.


  METHOD post.
    me->set_headers( ).
    data(lo_entity) = me->rest_client->if_rest_client~create_request_entity(
*        iv_multipart = ABAP_FALSE
    ).
    lo_entity->set_string_data( iv_data ).
    me->rest_client->if_rest_client~post( io_entity = lo_entity ).
  ENDMETHOD.


  METHOD set_headers.
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
  ENDMETHOD.


  METHOD test_connection.

    CONSTANTS: lc_match_path TYPE string VALUE '/ws/match/rest/v1.0/authorization-test'.

    me->rest_client->if_rest_client~set_request_header(
      EXPORTING
        iv_name  = if_http_header_fields_sap=>request_uri
        iv_value = lc_match_path
    ).
    me->get( ).
    DATA(lv_status) = me->rest_client->if_rest_client~get_status( ).
    IF lv_status = '200'.
      rv_ok = abap_true.
    ELSE.
      rv_ok = abap_false.
    ENDIF.
  ENDMETHOD.
ENDCLASS.