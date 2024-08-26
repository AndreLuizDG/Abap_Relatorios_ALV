REPORT z_algj_31.

*$*$ -------------------------------------------------------------- *$*$
*$*$                         DECLARAÇÕES                            *$*$
*$*$ -------------------------------------------------------------- *$*$

TYPE-POOLS: slis.

TABLES: vbrk,
        vbak.

TYPES:

  BEGIN OF ty_vbrk,
    vbeln TYPE vbrk-vbeln,
    fkdat TYPE vbrk-fkdat,
    kunrg TYPE vbrk-kunrg,
  END OF ty_vbrk,

  BEGIN OF ty_vbrp,
    vbeln TYPE vbrp-vbeln,
    posnr TYPE vbrp-posnr,
    matnr TYPE vbrp-matnr,
    fkimg TYPE vbrp-fkimg,
    vrkme TYPE vbrp-vrkme,
    netwr TYPE vbrp-netwr,
    aubel TYPE vbrp-aubel,
  END OF ty_vbrp,

  BEGIN OF ty_kna1,
    kunnr TYPE kna1-kunnr,
    name1 TYPE kna1-name1,
  END OF ty_kna1,

  BEGIN OF ty_vbak,
    vbeln TYPE vbak-vbeln,
  END OF ty_vbak,

  BEGIN OF ty_makt,
    matnr TYPE makt-matnr,
    spras TYPE makt-spras,
    maktx TYPE makt-maktx,
  END OF ty_makt,

  BEGIN OF ty_tabela_aux,
    vbeln TYPE ztb_ex3ap_alv-vbeln,
    posnr TYPE ztb_ex3ap_alv-posnr,
  END OF ty_tabela_aux,

  BEGIN OF ty_saida,
    vbeln       TYPE vbrk-vbeln,
    posnr       TYPE vbrp-posnr,
    kunnr_name1 TYPE char64,
    fkdat       TYPE vbrk-fkdat,
    matnr_maktx TYPE char64,
    fkimg       TYPE vbrp-fkimg,
    vrkme       TYPE vbrp-vrkme,
    netwr       TYPE vbrp-netwr,
    aubel       TYPE vbrp-aubel,
    status      TYPE icon-id,
    flag        TYPE flag,
    cellcolor   TYPE lvc_t_scol,
  END OF ty_saida.

DATA: ti_vbrk       TYPE TABLE OF ty_vbrk,
      ti_vbrp       TYPE TABLE OF ty_vbrp,
      ti_kna1       TYPE TABLE OF ty_kna1,
      ti_vbak       TYPE TABLE OF ty_vbak,
      ti_makt       TYPE TABLE OF ty_makt,
      ti_saida      TYPE TABLE OF ty_saida,
      ti_sort       TYPE TABLE OF slis_sortinfo_alv,
      ti_listheader TYPE TABLE OF slis_listheader,
      ti_fieldcat   TYPE TABLE OF slis_fieldcat_alv,
      ti_linecolor  TYPE TABLE OF slis_color,
      ti_tabela     TYPE TABLE OF ztb_ex3ap_alv,
      ti_tabela_aux TYPE TABLE OF ty_tabela_aux.

DATA:        wa_vbrk       TYPE ty_vbrk,
             wa_vbrp       TYPE ty_vbrp,
             wa_kna1       TYPE ty_kna1,
             wa_vbak       TYPE ty_vbak,
             wa_makt       TYPE ty_makt,
             wa_saida      TYPE ty_saida,
             wa_sort       TYPE slis_sortinfo_alv,
             wa_fieldcat   TYPE slis_fieldcat_alv,
             wa_listheader TYPE slis_listheader,
             wa_cellcolor  TYPE lvc_s_scol,
             wa_tabela     TYPE ztb_ex3ap_alv,
             wa_tabela_aux TYPE ty_tabela_aux.

CONSTANTS: c_f2                  TYPE char2  VALUE 'F2',
           c_s                   TYPE char1  VALUE 'S',
           c_e                   TYPE char1  VALUE 'E',
           c_x                   TYPE char1  VALUE 'X',
           c_aubel               TYPE char5  VALUE 'AUBEL',
           c_aun                 TYPE char3  VALUE 'AUN',
           c_va03                TYPE char4  VALUE 'VA03',
           c_vbeln               TYPE char5  VALUE 'VBELN',
           c_vf                  TYPE char2  VALUE 'VF',
           c_vf03                TYPE char4  VALUE 'VF03',
           c_gravar_reg          TYPE char10 VALUE 'GRAVAR_REG',
           c_standard_fullscreen TYPE char19 VALUE 'STANDARD_FULLSCREEN',
           c_separador1          TYPE char1  VALUE '/',
           c_separador2          TYPE char1  VALUE ':',
           c_separador3          TYPE char1  VALUE '|',
           c_kunnr_name1         TYPE char11 VALUE 'KUNNR_NAME1',
           c_ti_saida            TYPE char8  VALUE 'TI_SAIDA',
           c_fkdat               TYPE char5  VALUE 'FKDAT',
           c_zf_status           TYPE slis_formname VALUE 'ZF_STATUS',
           c_zf_user_command     TYPE slis_formname VALUE 'ZF_USER_COMMAND',
           c_zf_top_of_page      TYPE slis_formname VALUE 'ZF_TOP_OF_PAGE',
           c_flag                TYPE char4  VALUE 'FLAG',
           c_cellcolor           TYPE char9  VALUE 'CELLCOLOR',
           c_posnr               TYPE char5  VALUE 'POSNR',
           c_matnr_maktx         TYPE char11 VALUE 'MATNR_MAKTX',
           c_fkimg               TYPE char5  VALUE 'FKIMG',
           c_vrkme               TYPE char5  VALUE 'VRKME',
           c_netwr               TYPE char5  VALUE 'NETWR',
           c_status              TYPE char6  VALUE 'STATUS',
           c_id                  TYPE char2  VALUE 'ID',
           c_vbrk                TYPE char4  VALUE 'VBRK',
           c_vbrp                TYPE char4  VALUE 'VBRP',
           c_icon                TYPE char4  VALUE 'ICON',
           c_cliente             TYPE char7  VALUE 'CLIENTE',
           c_material            TYPE char8  VALUE 'MATERIAL'.
*$*$ -------------------------------------------------------------- *$*$
*$*$                            TELA                                *$*$
*$*$ -------------------------------------------------------------- *$*$

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001. "TELA DE SELEÇÃO:
SELECT-OPTIONS: s_vbeln FOR  vbrk-vbeln,
                s_fkdat FOR  vbrk-fkdat.
PARAMETERS:     p_kunnr TYPE vbak-kunnr.
SELECTION-SCREEN: END OF BLOCK b1.

*$*$ -------------------------------------------------------------- *$*$
*$*$                           EVENTOS                              *$*$
*$*$ -------------------------------------------------------------- *$*$

START-OF-SELECTION.
  PERFORM zf_seleciona_dados.

END-OF-SELECTION.
  PERFORM: zf_processa_dados,
           zf_monta_alv,
           zf_quebra_de_campo,
           zf_mostra_alv.

*$*$ -------------------------------------------------------------- *$*$
*$*$                            FORMS                               *$*$
*$*$ -------------------------------------------------------------- *$*$

FORM zf_seleciona_dados.

  FREE ti_vbrk.
  SELECT vbeln
         fkdat
         kunrg
    FROM vbrk
    INTO TABLE ti_vbrk
   WHERE vbeln IN s_vbeln
     AND fkart =  c_f2
     AND fkdat IN s_fkdat.
*     AND KUNRG =  P_KUNNR.

  IF sy-subrc <> 0.
    FREE ti_vbrk.
    MESSAGE text-002 TYPE c_s DISPLAY LIKE c_e.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF ti_vbrk IS NOT INITIAL.

    FREE ti_vbrp.
    SELECT vbeln
           posnr
           matnr
           fkimg
           vrkme
           netwr
           aubel
      FROM vbrp
      INTO TABLE ti_vbrp
       FOR ALL ENTRIES IN ti_vbrk
     WHERE vbeln = ti_vbrk-vbeln.

    IF sy-subrc <> 0.
      FREE ti_vbrp.
    ENDIF.

    DATA(ti_vbrk_aux) = ti_vbrk.
    SORT ti_vbrk_aux BY kunrg.
    DELETE ADJACENT DUPLICATES FROM ti_vbrk_aux COMPARING kunrg.

    FREE ti_kna1.
    SELECT kunnr
           name1
      FROM kna1
      INTO TABLE ti_kna1
       FOR ALL ENTRIES IN ti_vbrk_aux
     WHERE kunnr = ti_vbrk_aux-kunrg.

    IF sy-subrc <> 0.
      FREE ti_kna1.
    ENDIF.

  ENDIF.

  IF ti_vbrp IS NOT INITIAL.

    DATA(ti_vbrp_aux) = ti_vbrp.
    SORT ti_vbrp_aux BY aubel.
    DELETE ADJACENT DUPLICATES FROM ti_vbrp_aux COMPARING aubel.

    FREE ti_vbak.
    SELECT vbeln
      FROM vbak
      INTO TABLE ti_vbak
       FOR ALL ENTRIES IN ti_vbrp_aux
     WHERE vbeln = ti_vbrp_aux-aubel.

    IF sy-subrc <> 0.
      FREE ti_vbak.
    ENDIF.

    FREE ti_vbrp_aux.
    ti_vbrp_aux = ti_vbrp.
    SORT: ti_vbrp_aux BY matnr.
    DELETE ADJACENT DUPLICATES FROM ti_vbrp_aux COMPARING matnr.

    FREE ti_makt.
    SELECT matnr
           spras
           maktx
      FROM makt
      INTO TABLE ti_makt
       FOR ALL ENTRIES IN ti_vbrp_aux
     WHERE matnr = ti_vbrp_aux-matnr
       AND spras = sy-langu.

    IF sy-subrc <> 0.
      FREE ti_makt.
    ENDIF.

  ENDIF.
ENDFORM.

FORM zf_processa_dados.

  SORT: ti_vbrk BY vbeln,
        ti_vbrp BY vbeln,
        ti_vbrp BY vbeln,
        ti_kna1 BY kunnr,
        ti_makt BY matnr.


  LOOP AT ti_vbrp INTO wa_vbrp.

    CLEAR wa_vbrk.
    READ TABLE  ti_vbrk INTO wa_vbrk
                     WITH KEY vbeln = wa_vbrp-vbeln BINARY SEARCH.

    CLEAR wa_vbak.
    READ TABLE ti_vbak INTO wa_vbak
                     WITH KEY vbeln = wa_vbrp-aubel  BINARY SEARCH.

    CLEAR wa_kna1.
    READ TABLE ti_kna1 INTO wa_kna1
                     WITH KEY kunnr = wa_vbrk-kunrg BINARY SEARCH.

    CLEAR wa_makt.
    READ TABLE ti_makt INTO wa_makt
                     WITH KEY matnr = wa_makt-matnr BINARY SEARCH.

    CONCATENATE wa_vbrk-kunrg wa_kna1-name1
    INTO wa_saida-kunnr_name1
    SEPARATED BY '-'.

    CONCATENATE wa_vbrp-matnr wa_makt-maktx
    INTO wa_saida-matnr_maktx
    SEPARATED BY '-'.

    CLEAR wa_saida.

    wa_saida-vbeln = wa_vbrk-vbeln.
    wa_saida-posnr = wa_vbrp-posnr.
    wa_saida-fkdat = wa_vbrk-fkdat.
    wa_saida-fkimg = wa_vbrp-fkimg.
    wa_saida-vrkme = wa_vbrp-vrkme.
    wa_saida-netwr = wa_vbrp-netwr.
    wa_saida-aubel = wa_vbrp-aubel.

    IF wa_saida-fkimg < 10.
      wa_cellcolor-fname = c_fkimg.
      wa_cellcolor-color-col = 5.
    ELSE.
      wa_cellcolor-fname = c_fkimg.
      wa_cellcolor-color-col = 6.
    ENDIF.

    APPEND wa_cellcolor TO wa_saida-cellcolor.

    IF wa_saida-netwr < 1000.
      wa_cellcolor-fname = c_netwr.
      wa_cellcolor-color-col = 5.
    ELSE.
      wa_cellcolor-fname = c_netwr.
      wa_cellcolor-color-col = 6.
    ENDIF.

    APPEND wa_cellcolor TO wa_saida-cellcolor.

    APPEND wa_saida TO ti_saida.

  ENDLOOP.


ENDFORM. "ZF_PROCESSA_DADOS


FORM zf_monta_alv.

  PERFORM zf_monta_fieldcat USING:
   c_vbeln          c_ti_saida  c_vbeln  c_vbrk  '' ''  '',
   c_posnr          c_ti_saida  c_posnr  c_vbrp  '' ''  '',
   c_kunnr_name1    c_ti_saida  ''       ''      '' ''  c_cliente,
   c_fkdat          c_ti_saida  c_fkdat  c_vbrk  '' ''  '',
   c_matnr_maktx    c_ti_saida  ''       ''      '' ''  c_material,
   c_fkimg          c_ti_saida  c_fkimg  c_vbrp  '' c_x '',
   c_vrkme          c_ti_saida  c_vrkme  c_vbrp  '' ''  '',
   c_netwr          c_ti_saida  c_netwr  c_vbrp  '' c_x '',
   c_aubel          c_ti_saida  c_aubel  c_vbrp  '' ''  '',
   c_status         c_ti_saida  c_id     c_icon  '' ''  c_status.

ENDFORM. "ZF_MONTA_ALV

FORM zf_monta_fieldcat USING field        TYPE any
                             tab          TYPE any
                             ref_field    TYPE any
                             ref_tab      TYPE any
                             hotspot      TYPE any
                             sum          TYPE any
                             reptext_ddic TYPE any.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname     = field.
  wa_fieldcat-tabname       = tab.
  wa_fieldcat-ref_fieldname = ref_field.
  wa_fieldcat-ref_tabname   = ref_tab.
  wa_fieldcat-hotspot       = hotspot.
  wa_fieldcat-do_sum        = sum.
  wa_fieldcat-reptext_ddic  = reptext_ddic.

  APPEND wa_fieldcat TO ti_fieldcat.

ENDFORM. "ZF_MONTA_FIELDCAT

FORM zf_mostra_alv.

  DATA: wa_layout TYPE slis_layout_alv.

  wa_layout-expand_all        = abap_true.
  wa_layout-colwidth_optimize = abap_true.
  wa_layout-zebra             = abap_true.
  wa_layout-box_fieldname     = c_flag.
  wa_layout-coltab_fieldname  = c_cellcolor.



  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = c_zf_status
      i_callback_user_command  = c_zf_user_command
      i_callback_top_of_page   = c_zf_top_of_page
      is_layout                = wa_layout
      it_fieldcat              = ti_fieldcat
      it_sort                  = ti_sort
    TABLES
      t_outtab                 = ti_saida
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE text-003 TYPE c_s DISPLAY LIKE c_e.
    LEAVE LIST-PROCESSING.
  ENDIF.


ENDFORM. "ZF_MOSTRA_ALV

FORM zf_quebra_de_campo.

  FREE ti_sort.

  CLEAR wa_sort.
  wa_sort-spos      = 1.
  wa_sort-fieldname = c_kunnr_name1.
  wa_sort-tabname   = c_ti_saida.
  wa_sort-up        = abap_true.
  wa_sort-subtot    = abap_true.
  APPEND wa_sort TO ti_sort.

  CLEAR wa_sort.
  wa_sort-spos      = 2.
  wa_sort-fieldname = c_fkdat.
  wa_sort-tabname   = c_ti_saida.
  wa_sort-up        = abap_true.
  wa_sort-subtot    = abap_true.
  APPEND wa_sort TO ti_sort.

ENDFORM. "FORM ZF_QUEBRA_DE_CAMPO

FORM zf_top_of_page.

  DATA: data      TYPE char10,
        hora      TYPE char5,
        timestamp TYPE char30.

  FREE ti_listheader.

  CONCATENATE sy-datum+6(2)
              sy-datum+4(2)
              sy-datum+0(4)
             INTO  data SEPARATED BY c_separador1.

  CONCATENATE sy-uzeit+0(2)
              sy-uzeit+2(2)
             INTO hora SEPARATED BY c_separador2.

  CONCATENATE sy-repid c_separador3 data hora INTO timestamp SEPARATED BY space.

  CLEAR wa_listheader.
  wa_listheader-typ  = 'S'.
  wa_listheader-info = timestamp.
  APPEND wa_listheader TO ti_listheader.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = ti_listheader.


ENDFORM. "ZF_TOP_OF_PAGE

FORM zf_status USING pf_tab TYPE slis_t_extab.
  SET PF-STATUS c_standard_fullscreen.
ENDFORM. "ZF_STATUS

FORM zf_user_command USING vl_ucomm    LIKE sy-ucomm
                           rs_selfield TYPE slis_selfield.

  rs_selfield-refresh = abap_true.

  CASE vl_ucomm.

    WHEN c_gravar_reg.

      REFRESH ti_tabela.

      SELECT vbeln
             posnr
          INTO TABLE ti_tabela_aux
          FROM ztb_ex3ap_alv
          FOR ALL ENTRIES IN ti_saida
          WHERE vbeln = ti_saida-vbeln AND
                posnr = ti_saida-posnr.

      IF sy-subrc EQ 0.
        SORT: ti_tabela_aux BY vbeln
                               posnr.
      ENDIF.

      LOOP AT ti_saida INTO wa_saida WHERE flag = 'X'.

        wa_tabela-vbeln       = wa_saida-vbeln      .
        wa_tabela-posnr       = wa_saida-posnr      .
        wa_tabela-kunrg_name1 = wa_saida-kunnr_name1.
        wa_tabela-fkdat       = wa_saida-fkdat      .
        wa_tabela-matnr_maktx = wa_saida-matnr_maktx.
        wa_tabela-fkimg       = wa_saida-fkimg      .
        wa_tabela-vrkme       = wa_saida-vrkme      .
        wa_tabela-netwr       = wa_saida-netwr      .
        wa_tabela-aubel       = wa_saida-aubel      .

        READ TABLE ti_tabela_aux TRANSPORTING NO FIELDS
                                 WITH KEY vbeln = wa_saida-vbeln
                                          posnr = wa_saida-posnr BINARY SEARCH.

        IF sy-subrc IS NOT INITIAL.
          APPEND wa_tabela TO ti_tabela.
          wa_saida-status = icon_green_light.
        ELSE.
          wa_saida-status = icon_red_light.
        ENDIF.

        MODIFY ti_saida FROM wa_saida.

      ENDLOOP.

      MODIFY ztb_ex3ap_alv FROM TABLE ti_tabela.
    WHEN OTHERS.

      IF rs_selfield-fieldname = c_aubel.
        SET PARAMETER ID c_aun FIELD rs_selfield-value.
        CALL TRANSACTION c_va03 AND SKIP FIRST SCREEN.

      ELSEIF rs_selfield-fieldname = c_vbeln.
        SET PARAMETER ID c_vf FIELD rs_selfield-value.
        CALL TRANSACTION c_vf03 AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.

ENDFORM. "Z_USER_COMMAND