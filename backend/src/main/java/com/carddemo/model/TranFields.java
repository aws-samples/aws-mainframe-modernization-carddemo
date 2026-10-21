package com.carddemo.model;

import jakarta.persistence.Column;
import jakarta.persistence.MappedSuperclass;
import lombok.Getter;
import lombok.Setter;

import java.math.BigDecimal;

/** Shared field layout between TRAN-RECORD (CVTRA05Y) and
 *  DALYTRAN-RECORD (CVTRA06Y). */
@MappedSuperclass
@Getter
@Setter
public abstract class TranFields {

    /** TRAN-TYPE-CD PIC X(02). */
    @Column(name = "type_cd", length = 2)
    private String typeCd;

    /** TRAN-CAT-CD PIC 9(04). */
    @Column(name = "cat_cd")
    private Integer catCd;

    /** TRAN-SOURCE PIC X(10). */
    @Column(name = "source", length = 10)
    private String source;

    /** TRAN-DESC PIC X(100). */
    @Column(name = "descr", length = 100)
    private String description;

    /** TRAN-AMT PIC S9(09)V99. */
    @Column(name = "amt", precision = 11, scale = 2)
    private BigDecimal amt = BigDecimal.ZERO.setScale(2);

    /** TRAN-MERCHANT-ID PIC 9(09). */
    @Column(name = "merchant_id")
    private Long merchantId;

    /** TRAN-MERCHANT-NAME PIC X(50). */
    @Column(name = "merchant_name", length = 50)
    private String merchantName;

    /** TRAN-MERCHANT-CITY PIC X(50). */
    @Column(name = "merchant_city", length = 50)
    private String merchantCity;

    /** TRAN-MERCHANT-ZIP PIC X(10). */
    @Column(name = "merchant_zip", length = 10)
    private String merchantZip;

    /** TRAN-CARD-NUM PIC X(16). */
    @Column(name = "card_num", length = 16)
    private String cardNum;

    /** TRAN-ORIG-TS PIC X(26) (YYYY-MM-DD HH:MM:SS.ffffff). */
    @Column(name = "orig_ts", length = 26)
    private String origTs;

    /** TRAN-PROC-TS PIC X(26). */
    @Column(name = "proc_ts", length = 26)
    private String procTs;

    /** Copy every non-key field from another record (DALYTRAN -> TRAN). */
    public void copyFieldsFrom(TranFields src) {
        setTypeCd(src.getTypeCd());
        setCatCd(src.getCatCd());
        setSource(src.getSource());
        setDescription(src.getDescription());
        setAmt(src.getAmt());
        setMerchantId(src.getMerchantId());
        setMerchantName(src.getMerchantName());
        setMerchantCity(src.getMerchantCity());
        setMerchantZip(src.getMerchantZip());
        setCardNum(src.getCardNum());
        setOrigTs(src.getOrigTs());
        setProcTs(src.getProcTs());
    }
}
