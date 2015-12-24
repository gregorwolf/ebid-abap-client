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
      genterate_gguid FOR TESTING RAISING cx_static_check.

    DATA: lo_ebid TYPE REF TO zcl_ebid,
          ex      TYPE REF TO zcx_ebid.

ENDCLASS.


CLASS ltcl_ebid IMPLEMENTATION.

  METHOD instanciate_ebid.

    TRY.
        CREATE OBJECT lo_ebid.
        cl_aunit_assert=>assert_bound(
          EXPORTING
            act              = lo_ebid  " Reference Variable to Be Checked
        ).
        CLEAR: lo_ebid.
        CREATE OBJECT lo_ebid
          EXPORTING
            iv_destination = 'NON_EXISTING_EBID_DESTINATON'.
      CATCH zcx_ebid INTO ex.
        cl_aunit_assert=>assert_bound(
          EXPORTING
            act              = ex   " Reference Variable to Be Checked
        ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_connection.

    CREATE OBJECT lo_ebid.

    DATA(lv_ok) = lo_ebid->test_connection( ).
    cl_aunit_assert=>assert_equals(
      EXPORTING
        exp                  = abap_true    " Data Object with Expected Type
        act                  = lv_ok   " Data Object with Current Value
    ).

  ENDMETHOD.

  METHOD genterate_gguid.

    DATA: lv_gguid type SUID_UUID.

    lv_gguid = zcl_ebid=>get_gguid( ).
    cl_aunit_assert=>assert_not_initial(
      EXPORTING
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
        lt_match_res = lo_ebid->match( ls_match_req ).
      CATCH zcx_ebid INTO DATA(ex).
        cl_aunit_assert=>assert_bound(
          EXPORTING
            act              = ex   " Reference Variable to Be Checked
        ).
    ENDTRY.

    CLEAR: lo_ebid.
    CREATE OBJECT lo_ebid.
    ls_match_req-street       = 'Marcel-Breuer-Str. 6'.
    lt_match_res = lo_ebid->match( ls_match_req ).

    cl_aunit_assert=>assert_not_initial(
      EXPORTING
        act              = lt_match_res   " Actual Data Object
    ).

    DESCRIBE TABLE lt_match_res LINES DATA(lv_lines).

    cl_aunit_assert=>assert_equals(
      EXPORTING
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
          EXPORTING
            act              = ex   " Reference Variable to Be Checked
        ).
    ENDTRY.

    CLEAR: lo_ebid, ls_company_res.
    CREATE OBJECT lo_ebid.
    lv_ebid = '2508838560133'.
    ls_company_res = lo_ebid->get_company( lv_ebid ).

    cl_aunit_assert=>assert_not_initial(
      EXPORTING
        act              = ls_company_res   " Actual Data Object
    ).

  ENDMETHOD.

ENDCLASS.