*"* use this source file for your ABAP unit test classes
CLASS ltcl_ebid DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      instanciate_ebid FOR TESTING RAISING cx_static_check,
      test_connection FOR TESTING RAISING cx_static_check,
      get_match FOR TESTING RAISING cx_static_check,
      get_company FOR TESTING RAISING cx_static_check,
      search FOR TESTING RAISING cx_static_check,
      search_as_you_type FOR TESTING RAISING cx_static_check,
      genterate_gguid FOR TESTING RAISING cx_static_check.

    DATA: lo_ebid TYPE REF TO zcl_ebid,
          ex      TYPE REF TO zcx_ebid.

ENDCLASS.


CLASS ltcl_ebid IMPLEMENTATION.

  METHOD instanciate_ebid.

    TRY.
      CREATE OBJECT lo_ebid.
      cl_aunit_assert=>assert_bound(
        act              = lo_ebid  " Reference Variable to Be Checked
      ).

      CLEAR: lo_ebid.
      CREATE OBJECT lo_ebid
        EXPORTING
          iv_destination = 'NON_EXISTING_EBID_DESTINATON'.

    CATCH zcx_ebid INTO ex.
      cl_aunit_assert=>assert_bound(
        act              = ex   " Reference Variable to Be Checked
      ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_connection.

    CREATE OBJECT lo_ebid.

    DATA(lv_ok) = lo_ebid->test_connection( ).
    cl_aunit_assert=>assert_equals(
      exp                  = abap_true    " Data Object with Expected Type
      act                  = lv_ok   " Data Object with Current Value
    ).
    " second call should not fail
    lv_ok = lo_ebid->test_connection( ).
    cl_aunit_assert=>assert_equals(
      exp                  = abap_true    " Data Object with Expected Type
      act                  = lv_ok   " Data Object with Current Value
    ).

  ENDMETHOD.

  METHOD genterate_gguid.

    DATA: lv_gguid TYPE suid_uuid.

    lv_gguid = zcl_ebid=>get_gguid( ).
    cl_aunit_assert=>assert_not_initial(
      act              = lv_gguid    " Actual Data Object
    ).

  ENDMETHOD.

  METHOD get_match.
    DATA: ls_match_req TYPE zebid_match_request,
          lt_match_res TYPE zebid_match_response_t.

    CREATE OBJECT lo_ebid.

    ls_match_req-city         = 'München'.
    ls_match_req-company_name = 'OSRAM GmbH'.

    TRY.
        lo_ebid->match(
          EXPORTING
            is_match_request = ls_match_req
          IMPORTING
            rt_match_response = lt_match_res
        ).
      CATCH zcx_ebid INTO DATA(ex).
        cl_aunit_assert=>assert_bound(
          act              = ex   " Reference Variable to Be Checked
        ).
    ENDTRY.

    ls_match_req-street       = 'Marcel-Breuer-Str. 6'.
    TRY.
        lo_ebid->match(
          EXPORTING
            is_match_request = ls_match_req
          IMPORTING
            rt_match_response = lt_match_res
        ).
      CATCH zcx_ebid INTO ex.
        cl_aunit_assert=>assert_not_bound(
          act              = ex    " Reference Variable to Be Checked
          msg              = ex->get_text( )   " Error Message
        ).
    ENDTRY.

    cl_aunit_assert=>assert_not_initial(
      act              = lt_match_res   " Actual Data Object
    ).

    DESCRIBE TABLE lt_match_res LINES DATA(lv_lines).

    cl_aunit_assert=>assert_equals(
      exp                  =  1   " Data Object with Expected Type
      act                  =  lv_lines   " Data Object with Current Value
    ).

  ENDMETHOD.

  METHOD get_company.
    DATA: lv_ebid        TYPE string VALUE '508838560133', " Invalid EBID
          ls_company_res TYPE zebid_company_response.

    CREATE OBJECT lo_ebid.

    TRY.
        ls_company_res = lo_ebid->get_company( lv_ebid ).
      CATCH zcx_ebid INTO DATA(ex).
        cl_aunit_assert=>assert_bound(
          act              = ex   " Reference Variable to Be Checked
        ).
    ENDTRY.

    lv_ebid = '2508838560133'.
    ls_company_res = lo_ebid->get_company( lv_ebid ).

    cl_aunit_assert=>assert_not_initial(
      act              = ls_company_res   " Actual Data Object
    ).

  ENDMETHOD.

  METHOD search.
    DATA: ls_search_req TYPE zebid_match_request,
          ls_search_res TYPE zebid_search_response.

    CREATE OBJECT lo_ebid.
    ls_search_req-company_name = 'Bosch Siemens'.
    ls_search_req-city         = 'München'.

    lo_ebid->search(
      EXPORTING
        is_search_request  = ls_search_req
      IMPORTING
        rs_search_response = ls_search_res
    ).

    cl_aunit_assert=>assert_not_initial(
      act              = ls_search_res   " Actual Data Object
    ).

    DESCRIBE TABLE ls_search_res-suggestion LINES DATA(lv_lines).

    cl_aunit_assert=>assert_equals(
      exp                  = 10    " Data Object with Expected Type
      act                  = lv_lines    " Data Object with Current Value
    ).

    CLEAR: ls_search_res.

    ls_search_req-company_name = 'OSRAM GmbH'.
    ls_search_req-city         = 'München'.

    lo_ebid->search(
      EXPORTING
        is_search_request  = ls_search_req
      IMPORTING
        rs_search_response = ls_search_res
    ).

  ENDMETHOD.

  METHOD search_as_you_type.
    DATA: lv_query TYPE string VALUE 'OSRA'.

    CREATE OBJECT lo_ebid.
    DATA(lt_result) = lo_ebid->search_as_you_type( lv_query ).

    cl_aunit_assert=>assert_not_initial(
      act              = lt_result   " Actual Data Object
    ).

    clear: lt_result.

    lv_query = 'SAP SE'.
    lt_result = lo_ebid->search_as_you_type( lv_query ).
    cl_aunit_assert=>assert_not_initial(
      act              = lt_result   " Actual Data Object
    ).


  ENDMETHOD.

ENDCLASS.