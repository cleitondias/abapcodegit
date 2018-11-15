*&---------------------------------------------------------------------*
*&  Include  zsd_reprocess_interface_sel
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS: p_ctt  RADIOBUTTON GROUP b1 USER-COMMAND trn DEFAULT 'X',
            p_tnb  RADIOBUTTON GROUP b1,
            p_chnr RADIOBUTTON GROUP b1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.
SELECT-OPTIONS: s_sales FOR vbak-vbeln MODIF ID ab,
                s_deliv FOR likp-vbeln MODIF ID fd,
                s_erdat FOR likp-erdat,
                s_vdatu for vbak-vdatu.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-b03.
PARAMETERS: p_send   AS CHECKBOX,
            p_nsend  AS CHECKBOX,
            p_errors AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b3.

***AT SELECTION-SCREEN OUTPUT.
***  LOOP AT SCREEN.
***    IF p_ctt IS NOT INITIAL OR
***       p_chnr IS NOT INITIAL.
***      IF screen-group1 EQ 'AB'.
***        screen-active = 0.
***      ENDIF.
***    ELSE.
***      IF screen-group1 EQ 'FD'.
***        screen-active = 0.
***      ENDIF.
***    ENDIF.
***    MODIFY SCREEN.
***  ENDLOOP.
