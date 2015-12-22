*"* use this source file for your ABAP unit test classes
CLASS ltcl_ebid DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      instanciate_ebid FOR TESTING RAISING cx_static_check,
      test_connection FOR TESTING RAISING cx_static_check.
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

ENDCLASS.