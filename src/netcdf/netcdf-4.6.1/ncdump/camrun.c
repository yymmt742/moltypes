#include <stdio.h>
#include <stdlib.h>
#include <netcdf.h>


static size_t time_chunksizes[1] = {1} ;
static size_t time_bnds_chunksizes[2] = {1, 2} ;
static size_t date_written_chunksizes[2] = {1, 8} ;
static size_t time_written_chunksizes[2] = {1, 8} ;
static size_t ndcur_chunksizes[1] = {1} ;
static size_t nscur_chunksizes[1] = {1} ;
static size_t date_chunksizes[1] = {1} ;
static size_t co2vmr_chunksizes[1] = {1} ;
static size_t ch4vmr_chunksizes[1] = {1} ;
static size_t n2ovmr_chunksizes[1] = {1} ;
static size_t f11vmr_chunksizes[1] = {1} ;
static size_t f12vmr_chunksizes[1] = {1} ;
static size_t sol_tsi_chunksizes[1] = {1} ;
static size_t datesec_chunksizes[1] = {1} ;
static size_t nsteph_chunksizes[1] = {1} ;
static size_t ABSORB_chunksizes[4] = {1, 30, 96, 144} ;
static size_t AEROD_v_chunksizes[3] = {1, 96, 144} ;
static size_t AODABS_chunksizes[3] = {1, 96, 144} ;
static size_t AODDUST1_chunksizes[3] = {1, 96, 144} ;
static size_t AODDUST2_chunksizes[3] = {1, 96, 144} ;
static size_t AODDUST3_chunksizes[3] = {1, 96, 144} ;
static size_t AODMODE1_chunksizes[3] = {1, 96, 144} ;
static size_t AODMODE2_chunksizes[3] = {1, 96, 144} ;
static size_t AODMODE3_chunksizes[3] = {1, 96, 144} ;
static size_t AODVIS_chunksizes[3] = {1, 96, 144} ;
static size_t AQSO4_H2O2_chunksizes[3] = {1, 96, 144} ;
static size_t AQSO4_O3_chunksizes[3] = {1, 96, 144} ;
static size_t AQ_DMS_chunksizes[3] = {1, 96, 144} ;
static size_t AQ_H2O2_chunksizes[3] = {1, 96, 144} ;
static size_t AQ_H2SO4_chunksizes[3] = {1, 96, 144} ;
static size_t AQ_SO2_chunksizes[3] = {1, 96, 144} ;
static size_t AQ_SOAG_chunksizes[3] = {1, 96, 144} ;
static size_t AQ_bc_a1_chunksizes[3] = {1, 96, 144} ;
static size_t AQ_dst_a1_chunksizes[3] = {1, 96, 144} ;
static size_t AQ_dst_a3_chunksizes[3] = {1, 96, 144} ;
static size_t AQ_ncl_a1_chunksizes[3] = {1, 96, 144} ;
static size_t AQ_ncl_a2_chunksizes[3] = {1, 96, 144} ;
static size_t AQ_ncl_a3_chunksizes[3] = {1, 96, 144} ;
static size_t AQ_num_a1_chunksizes[3] = {1, 96, 144} ;
static size_t AQ_num_a2_chunksizes[3] = {1, 96, 144} ;
static size_t AQ_num_a3_chunksizes[3] = {1, 96, 144} ;
static size_t AQ_pom_a1_chunksizes[3] = {1, 96, 144} ;
static size_t AQ_so4_a1_chunksizes[3] = {1, 96, 144} ;
static size_t AQ_so4_a2_chunksizes[3] = {1, 96, 144} ;
static size_t AQ_so4_a3_chunksizes[3] = {1, 96, 144} ;
static size_t AQ_soa_a1_chunksizes[3] = {1, 96, 144} ;
static size_t AQ_soa_a2_chunksizes[3] = {1, 96, 144} ;
static size_t BPROD_chunksizes[4] = {1, 31, 96, 144} ;
static size_t BURDEN1_chunksizes[3] = {1, 96, 144} ;
static size_t BURDEN2_chunksizes[3] = {1, 96, 144} ;
static size_t BURDEN3_chunksizes[3] = {1, 96, 144} ;
static size_t CCN1_chunksizes[4] = {1, 30, 96, 144} ;
static size_t CCN2_chunksizes[4] = {1, 30, 96, 144} ;
static size_t CCN3_chunksizes[4] = {1, 30, 96, 144} ;
static size_t CCN4_chunksizes[4] = {1, 30, 96, 144} ;
static size_t CCN5_chunksizes[4] = {1, 30, 96, 144} ;
static size_t CCN6_chunksizes[4] = {1, 30, 96, 144} ;
static size_t CLDHGH_chunksizes[3] = {1, 96, 144} ;
static size_t CLDICE_chunksizes[4] = {1, 30, 96, 144} ;
static size_t CLDLIQ_chunksizes[4] = {1, 30, 96, 144} ;
static size_t CLDLOW_chunksizes[3] = {1, 96, 144} ;
static size_t CLDMED_chunksizes[3] = {1, 96, 144} ;
static size_t CLDTOT_chunksizes[3] = {1, 96, 144} ;
static size_t CLOUD_chunksizes[4] = {1, 30, 96, 144} ;
static size_t CMFDQ_chunksizes[4] = {1, 30, 96, 144} ;
static size_t CMFDQR_chunksizes[4] = {1, 30, 96, 144} ;
static size_t CMFDT_chunksizes[4] = {1, 30, 96, 144} ;
static size_t CMFMC_chunksizes[4] = {1, 31, 96, 144} ;
static size_t CMFMCDZM_chunksizes[4] = {1, 31, 96, 144} ;
static size_t CONCLD_chunksizes[4] = {1, 30, 96, 144} ;
static size_t DCQ_chunksizes[4] = {1, 30, 96, 144} ;
static size_t DMS_chunksizes[4] = {1, 30, 96, 144} ;
static size_t DSTODXC_chunksizes[3] = {1, 96, 144} ;
static size_t DSTSFDRY_chunksizes[3] = {1, 96, 144} ;
static size_t DSTSFMBL_chunksizes[3] = {1, 96, 144} ;
static size_t DSTSFWET_chunksizes[3] = {1, 96, 144} ;
static size_t DTCOND_chunksizes[4] = {1, 30, 96, 144} ;
static size_t DTV_chunksizes[4] = {1, 30, 96, 144} ;
static size_t EXTINCT_chunksizes[4] = {1, 30, 96, 144} ;
static size_t FICE_chunksizes[4] = {1, 30, 96, 144} ;
static size_t FLDS_chunksizes[3] = {1, 96, 144} ;
static size_t FLNS_chunksizes[3] = {1, 96, 144} ;
static size_t FLNSC_chunksizes[3] = {1, 96, 144} ;
static size_t FLNT_chunksizes[3] = {1, 96, 144} ;
static size_t FLNTC_chunksizes[3] = {1, 96, 144} ;
static size_t FLUT_chunksizes[3] = {1, 96, 144} ;
static size_t FLUTC_chunksizes[3] = {1, 96, 144} ;
static size_t FREQSH_chunksizes[3] = {1, 96, 144} ;
static size_t FREQZM_chunksizes[3] = {1, 96, 144} ;
static size_t FSDS_chunksizes[3] = {1, 96, 144} ;
static size_t FSDSC_chunksizes[3] = {1, 96, 144} ;
static size_t FSNS_chunksizes[3] = {1, 96, 144} ;
static size_t FSNSC_chunksizes[3] = {1, 96, 144} ;
static size_t FSNT_chunksizes[3] = {1, 96, 144} ;
static size_t FSNTC_chunksizes[3] = {1, 96, 144} ;
static size_t FSNTOA_chunksizes[3] = {1, 96, 144} ;
static size_t FSNTOAC_chunksizes[3] = {1, 96, 144} ;
static size_t FSUTOA_chunksizes[3] = {1, 96, 144} ;
static size_t GS_DMS_chunksizes[3] = {1, 96, 144} ;
static size_t GS_H2O2_chunksizes[3] = {1, 96, 144} ;
static size_t GS_H2SO4_chunksizes[3] = {1, 96, 144} ;
static size_t GS_SO2_chunksizes[3] = {1, 96, 144} ;
static size_t GS_SOAG_chunksizes[3] = {1, 96, 144} ;
static size_t GS_bc_a1_chunksizes[3] = {1, 96, 144} ;
static size_t GS_dst_a1_chunksizes[3] = {1, 96, 144} ;
static size_t GS_dst_a3_chunksizes[3] = {1, 96, 144} ;
static size_t GS_ncl_a1_chunksizes[3] = {1, 96, 144} ;
static size_t GS_ncl_a2_chunksizes[3] = {1, 96, 144} ;
static size_t GS_ncl_a3_chunksizes[3] = {1, 96, 144} ;
static size_t GS_num_a1_chunksizes[3] = {1, 96, 144} ;
static size_t GS_num_a2_chunksizes[3] = {1, 96, 144} ;
static size_t GS_num_a3_chunksizes[3] = {1, 96, 144} ;
static size_t GS_pom_a1_chunksizes[3] = {1, 96, 144} ;
static size_t GS_so4_a1_chunksizes[3] = {1, 96, 144} ;
static size_t GS_so4_a2_chunksizes[3] = {1, 96, 144} ;
static size_t GS_so4_a3_chunksizes[3] = {1, 96, 144} ;
static size_t GS_soa_a1_chunksizes[3] = {1, 96, 144} ;
static size_t GS_soa_a2_chunksizes[3] = {1, 96, 144} ;
static size_t H2O2_chunksizes[4] = {1, 30, 96, 144} ;
static size_t H2SO4_chunksizes[4] = {1, 30, 96, 144} ;
static size_t H2SO4_sfgaex1_chunksizes[3] = {1, 96, 144} ;
static size_t H2SO4_sfnnuc1_chunksizes[3] = {1, 96, 144} ;
static size_t ICEFRAC_chunksizes[3] = {1, 96, 144} ;
static size_t ICIMR_chunksizes[4] = {1, 30, 96, 144} ;
static size_t ICWMR_chunksizes[4] = {1, 30, 96, 144} ;
static size_t KVH_chunksizes[4] = {1, 31, 96, 144} ;
static size_t KVM_chunksizes[4] = {1, 31, 96, 144} ;
static size_t LANDFRAC_chunksizes[3] = {1, 96, 144} ;
static size_t LCLOUD_chunksizes[4] = {1, 30, 96, 144} ;
static size_t LHFLX_chunksizes[3] = {1, 96, 144} ;
static size_t LND_MBL_chunksizes[3] = {1, 96, 144} ;
static size_t LWCF_chunksizes[3] = {1, 96, 144} ;
static size_t NDROPCOL_chunksizes[3] = {1, 96, 144} ;
static size_t NDROPMIX_chunksizes[4] = {1, 30, 96, 144} ;
static size_t NDROPSNK_chunksizes[4] = {1, 30, 96, 144} ;
static size_t NDROPSRC_chunksizes[4] = {1, 30, 96, 144} ;
static size_t NUMICE_chunksizes[4] = {1, 30, 96, 144} ;
static size_t NUMLIQ_chunksizes[4] = {1, 30, 96, 144} ;
static size_t OCNFRAC_chunksizes[3] = {1, 96, 144} ;
static size_t ODV_bc_a1_chunksizes[3] = {1, 96, 144} ;
static size_t ODV_dst_a1_chunksizes[3] = {1, 96, 144} ;
static size_t ODV_dst_a3_chunksizes[3] = {1, 96, 144} ;
static size_t ODV_ncl_a1_chunksizes[3] = {1, 96, 144} ;
static size_t ODV_ncl_a3_chunksizes[3] = {1, 96, 144} ;
static size_t ODV_pom_a1_chunksizes[3] = {1, 96, 144} ;
static size_t ODV_so4_a1_chunksizes[3] = {1, 96, 144} ;
static size_t ODV_soa_a1_chunksizes[3] = {1, 96, 144} ;
static size_t OMEGA_chunksizes[4] = {1, 30, 96, 144} ;
static size_t OMEGAT_chunksizes[4] = {1, 30, 96, 144} ;
static size_t ORO_chunksizes[3] = {1, 96, 144} ;
static size_t PBLH_chunksizes[3] = {1, 96, 144} ;
static size_t PCONVB_chunksizes[3] = {1, 96, 144} ;
static size_t PCONVT_chunksizes[3] = {1, 96, 144} ;
static size_t PHIS_chunksizes[3] = {1, 96, 144} ;
static size_t PRECC_chunksizes[3] = {1, 96, 144} ;
static size_t PRECCDZM_chunksizes[3] = {1, 96, 144} ;
static size_t PRECL_chunksizes[3] = {1, 96, 144} ;
static size_t PRECSC_chunksizes[3] = {1, 96, 144} ;
static size_t PRECSH_chunksizes[3] = {1, 96, 144} ;
static size_t PRECSL_chunksizes[3] = {1, 96, 144} ;
static size_t PRECT_chunksizes[3] = {1, 96, 144} ;
static size_t PS_chunksizes[3] = {1, 96, 144} ;
static size_t PSL_chunksizes[3] = {1, 96, 144} ;
static size_t Q_chunksizes[4] = {1, 30, 96, 144} ;
static size_t QC_chunksizes[4] = {1, 30, 96, 144} ;
static size_t QFLX_chunksizes[3] = {1, 96, 144} ;
static size_t QREFHT_chunksizes[3] = {1, 96, 144} ;
static size_t QRL_chunksizes[4] = {1, 30, 96, 144} ;
static size_t QRS_chunksizes[4] = {1, 30, 96, 144} ;
static size_t QT_chunksizes[4] = {1, 30, 96, 144} ;
static size_t QTFLX_chunksizes[4] = {1, 31, 96, 144} ;
static size_t RAM1_chunksizes[3] = {1, 96, 144} ;
static size_t RELHUM_chunksizes[4] = {1, 30, 96, 144} ;
static size_t RHREFHT_chunksizes[3] = {1, 96, 144} ;
static size_t SFCLDICE_chunksizes[3] = {1, 96, 144} ;
static size_t SFCLDLIQ_chunksizes[3] = {1, 96, 144} ;
static size_t SFI_chunksizes[4] = {1, 31, 96, 144} ;
static size_t SFNUMICE_chunksizes[3] = {1, 96, 144} ;
static size_t SFNUMLIQ_chunksizes[3] = {1, 96, 144} ;
static size_t SHFLX_chunksizes[3] = {1, 96, 144} ;
static size_t SL_chunksizes[4] = {1, 30, 96, 144} ;
static size_t SLFLX_chunksizes[4] = {1, 31, 96, 144} ;
static size_t SLV_chunksizes[4] = {1, 30, 96, 144} ;
static size_t SNOWHICE_chunksizes[3] = {1, 96, 144} ;
static size_t SNOWHLND_chunksizes[3] = {1, 96, 144} ;
static size_t SO2_chunksizes[4] = {1, 30, 96, 144} ;
static size_t SO2_CLXF_chunksizes[3] = {1, 96, 144} ;
static size_t SO2_XFRC_chunksizes[4] = {1, 30, 96, 144} ;
static size_t SOAG_chunksizes[4] = {1, 30, 96, 144} ;
static size_t SOAG_sfgaex1_chunksizes[3] = {1, 96, 144} ;
static size_t SOLIN_chunksizes[3] = {1, 96, 144} ;
static size_t SPROD_chunksizes[4] = {1, 31, 96, 144} ;
static size_t SRFRAD_chunksizes[3] = {1, 96, 144} ;
static size_t SSAVIS_chunksizes[3] = {1, 96, 144} ;
static size_t SSTODXC_chunksizes[3] = {1, 96, 144} ;
static size_t SSTSFDRY_chunksizes[3] = {1, 96, 144} ;
static size_t SSTSFMBL_chunksizes[3] = {1, 96, 144} ;
static size_t SSTSFWET_chunksizes[3] = {1, 96, 144} ;
static size_t SWCF_chunksizes[3] = {1, 96, 144} ;
static size_t T_chunksizes[4] = {1, 30, 96, 144} ;
static size_t TAUTMSX_chunksizes[3] = {1, 96, 144} ;
static size_t TAUTMSY_chunksizes[3] = {1, 96, 144} ;
static size_t TAUX_chunksizes[3] = {1, 96, 144} ;
static size_t TAUY_chunksizes[3] = {1, 96, 144} ;
static size_t TGCLDCWP_chunksizes[3] = {1, 96, 144} ;
static size_t TGCLDIWP_chunksizes[3] = {1, 96, 144} ;
static size_t TGCLDLWP_chunksizes[3] = {1, 96, 144} ;
static size_t TKE_chunksizes[4] = {1, 31, 96, 144} ;
static size_t TMQ_chunksizes[3] = {1, 96, 144} ;
static size_t TREFHT_chunksizes[3] = {1, 96, 144} ;
static size_t TREFMNAV_chunksizes[3] = {1, 96, 144} ;
static size_t TREFMXAV_chunksizes[3] = {1, 96, 144} ;
static size_t TROP_FD_chunksizes[3] = {1, 96, 144} ;
static size_t TROP_P_chunksizes[3] = {1, 96, 144} ;
static size_t TROP_PD_chunksizes[4] = {1, 30, 96, 144} ;
static size_t TROP_T_chunksizes[3] = {1, 96, 144} ;
static size_t TROP_Z_chunksizes[3] = {1, 96, 144} ;
static size_t TS_chunksizes[3] = {1, 96, 144} ;
static size_t TSMN_chunksizes[3] = {1, 96, 144} ;
static size_t TSMX_chunksizes[3] = {1, 96, 144} ;
static size_t U_chunksizes[4] = {1, 30, 96, 144} ;
static size_t UFLX_chunksizes[4] = {1, 31, 96, 144} ;
static size_t US_chunksizes[4] = {1, 30, 95, 144} ;
static size_t UU_chunksizes[4] = {1, 30, 96, 144} ;
static size_t V_chunksizes[4] = {1, 30, 96, 144} ;
static size_t VD01_chunksizes[4] = {1, 30, 96, 144} ;
static size_t VFLX_chunksizes[4] = {1, 31, 96, 144} ;
static size_t VQ_chunksizes[4] = {1, 30, 96, 144} ;
static size_t VS_chunksizes[4] = {1, 30, 96, 144} ;
static size_t VT_chunksizes[4] = {1, 30, 96, 144} ;
static size_t VU_chunksizes[4] = {1, 30, 96, 144} ;
static size_t VV_chunksizes[4] = {1, 30, 96, 144} ;
static size_t WGUSTD_chunksizes[3] = {1, 96, 144} ;
static size_t WTKE_chunksizes[4] = {1, 30, 96, 144} ;
static size_t XPH_LWC_chunksizes[4] = {1, 30, 96, 144} ;
static size_t Z3_chunksizes[4] = {1, 30, 96, 144} ;
static size_t airFV_chunksizes[3] = {1, 96, 144} ;
static size_t bc_a1_chunksizes[4] = {1, 30, 96, 144} ;
static size_t bc_a1DDF_chunksizes[3] = {1, 96, 144} ;
static size_t bc_a1GVF_chunksizes[3] = {1, 96, 144} ;
static size_t bc_a1SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t bc_a1SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t bc_a1SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t bc_a1SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t bc_a1SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t bc_a1TBF_chunksizes[3] = {1, 96, 144} ;
static size_t bc_a1_CLXF_chunksizes[3] = {1, 96, 144} ;
static size_t bc_a1_XFRC_chunksizes[4] = {1, 30, 96, 144} ;
static size_t bc_c1_chunksizes[4] = {1, 30, 96, 144} ;
static size_t bc_c1DDF_chunksizes[3] = {1, 96, 144} ;
static size_t bc_c1GVF_chunksizes[3] = {1, 96, 144} ;
static size_t bc_c1SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t bc_c1SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t bc_c1SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t bc_c1SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t bc_c1SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t bc_c1TBF_chunksizes[3] = {1, 96, 144} ;
static size_t chem_trop_chunksizes[4] = {1, 30, 96, 144} ;
static size_t chem_trop_tropop_chunksizes[4] = {1, 30, 96, 144} ;
static size_t dgnd_a01_chunksizes[4] = {1, 30, 96, 144} ;
static size_t dgnd_a02_chunksizes[4] = {1, 30, 96, 144} ;
static size_t dgnd_a03_chunksizes[4] = {1, 30, 96, 144} ;
static size_t dgnw_a01_chunksizes[4] = {1, 30, 96, 144} ;
static size_t dgnw_a02_chunksizes[4] = {1, 30, 96, 144} ;
static size_t dgnw_a03_chunksizes[4] = {1, 30, 96, 144} ;
static size_t dst_a1_chunksizes[4] = {1, 30, 96, 144} ;
static size_t dst_a1DDF_chunksizes[3] = {1, 96, 144} ;
static size_t dst_a1GVF_chunksizes[3] = {1, 96, 144} ;
static size_t dst_a1SF_chunksizes[3] = {1, 96, 144} ;
static size_t dst_a1SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t dst_a1SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t dst_a1SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t dst_a1SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t dst_a1SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t dst_a1TBF_chunksizes[3] = {1, 96, 144} ;
static size_t dst_a3_chunksizes[4] = {1, 30, 96, 144} ;
static size_t dst_a3DDF_chunksizes[3] = {1, 96, 144} ;
static size_t dst_a3GVF_chunksizes[3] = {1, 96, 144} ;
static size_t dst_a3SF_chunksizes[3] = {1, 96, 144} ;
static size_t dst_a3SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t dst_a3SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t dst_a3SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t dst_a3SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t dst_a3SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t dst_a3TBF_chunksizes[3] = {1, 96, 144} ;
static size_t dst_c1_chunksizes[4] = {1, 30, 96, 144} ;
static size_t dst_c1DDF_chunksizes[3] = {1, 96, 144} ;
static size_t dst_c1GVF_chunksizes[3] = {1, 96, 144} ;
static size_t dst_c1SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t dst_c1SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t dst_c1SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t dst_c1SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t dst_c1SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t dst_c1TBF_chunksizes[3] = {1, 96, 144} ;
static size_t dst_c3_chunksizes[4] = {1, 30, 96, 144} ;
static size_t dst_c3DDF_chunksizes[3] = {1, 96, 144} ;
static size_t dst_c3GVF_chunksizes[3] = {1, 96, 144} ;
static size_t dst_c3SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t dst_c3SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t dst_c3SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t dst_c3SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t dst_c3SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t dst_c3TBF_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a1_chunksizes[4] = {1, 30, 96, 144} ;
static size_t ncl_a1DDF_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a1GVF_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a1SF_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a1SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a1SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a1SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a1SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a1SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a1TBF_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a1_sfcoag1_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a1_sfcsiz3_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a1_sfcsiz4_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a1_sfgaex2_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a2_chunksizes[4] = {1, 30, 96, 144} ;
static size_t ncl_a2DDF_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a2GVF_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a2SF_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a2SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a2SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a2SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a2SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a2SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a2TBF_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a2_sfcoag1_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a2_sfcsiz3_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a2_sfcsiz4_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a2_sfgaex2_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a3_chunksizes[4] = {1, 30, 96, 144} ;
static size_t ncl_a3DDF_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a3GVF_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a3SF_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a3SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a3SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a3SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a3SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a3SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_a3TBF_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c1_chunksizes[4] = {1, 30, 96, 144} ;
static size_t ncl_c1DDF_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c1GVF_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c1SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c1SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c1SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c1SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c1SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c1TBF_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c1_sfcsiz3_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c1_sfcsiz4_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c1_sfgaex2_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c2_chunksizes[4] = {1, 30, 96, 144} ;
static size_t ncl_c2DDF_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c2GVF_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c2SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c2SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c2SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c2SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c2SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c2TBF_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c2_sfcsiz3_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c2_sfcsiz4_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c2_sfgaex2_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c3_chunksizes[4] = {1, 30, 96, 144} ;
static size_t ncl_c3DDF_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c3GVF_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c3SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c3SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c3SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c3SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c3SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t ncl_c3TBF_chunksizes[3] = {1, 96, 144} ;
static size_t num_a1_chunksizes[4] = {1, 30, 96, 144} ;
static size_t num_a1DDF_chunksizes[3] = {1, 96, 144} ;
static size_t num_a1GVF_chunksizes[3] = {1, 96, 144} ;
static size_t num_a1SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t num_a1SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t num_a1SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t num_a1SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t num_a1SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t num_a1TBF_chunksizes[3] = {1, 96, 144} ;
static size_t num_a1_CLXF_chunksizes[3] = {1, 96, 144} ;
static size_t num_a1_XFRC_chunksizes[4] = {1, 30, 96, 144} ;
static size_t num_a1_sfcoag1_chunksizes[3] = {1, 96, 144} ;
static size_t num_a1_sfcsiz1_chunksizes[3] = {1, 96, 144} ;
static size_t num_a1_sfcsiz2_chunksizes[3] = {1, 96, 144} ;
static size_t num_a1_sfcsiz3_chunksizes[3] = {1, 96, 144} ;
static size_t num_a1_sfcsiz4_chunksizes[3] = {1, 96, 144} ;
static size_t num_a1_sfgaex2_chunksizes[3] = {1, 96, 144} ;
static size_t num_a2_chunksizes[4] = {1, 30, 96, 144} ;
static size_t num_a2DDF_chunksizes[3] = {1, 96, 144} ;
static size_t num_a2GVF_chunksizes[3] = {1, 96, 144} ;
static size_t num_a2SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t num_a2SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t num_a2SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t num_a2SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t num_a2SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t num_a2TBF_chunksizes[3] = {1, 96, 144} ;
static size_t num_a2_CLXF_chunksizes[3] = {1, 96, 144} ;
static size_t num_a2_XFRC_chunksizes[4] = {1, 30, 96, 144} ;
static size_t num_a2_sfcoag1_chunksizes[3] = {1, 96, 144} ;
static size_t num_a2_sfcsiz1_chunksizes[3] = {1, 96, 144} ;
static size_t num_a2_sfcsiz2_chunksizes[3] = {1, 96, 144} ;
static size_t num_a2_sfcsiz3_chunksizes[3] = {1, 96, 144} ;
static size_t num_a2_sfcsiz4_chunksizes[3] = {1, 96, 144} ;
static size_t num_a2_sfgaex2_chunksizes[3] = {1, 96, 144} ;
static size_t num_a2_sfnnuc1_chunksizes[3] = {1, 96, 144} ;
static size_t num_a3_chunksizes[4] = {1, 30, 96, 144} ;
static size_t num_a3DDF_chunksizes[3] = {1, 96, 144} ;
static size_t num_a3GVF_chunksizes[3] = {1, 96, 144} ;
static size_t num_a3SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t num_a3SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t num_a3SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t num_a3SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t num_a3SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t num_a3TBF_chunksizes[3] = {1, 96, 144} ;
static size_t num_a3_sfcsiz1_chunksizes[3] = {1, 96, 144} ;
static size_t num_a3_sfcsiz2_chunksizes[3] = {1, 96, 144} ;
static size_t num_c1_chunksizes[4] = {1, 30, 96, 144} ;
static size_t num_c1DDF_chunksizes[3] = {1, 96, 144} ;
static size_t num_c1GVF_chunksizes[3] = {1, 96, 144} ;
static size_t num_c1SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t num_c1SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t num_c1SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t num_c1SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t num_c1SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t num_c1TBF_chunksizes[3] = {1, 96, 144} ;
static size_t num_c1_sfcsiz1_chunksizes[3] = {1, 96, 144} ;
static size_t num_c1_sfcsiz2_chunksizes[3] = {1, 96, 144} ;
static size_t num_c1_sfcsiz3_chunksizes[3] = {1, 96, 144} ;
static size_t num_c1_sfcsiz4_chunksizes[3] = {1, 96, 144} ;
static size_t num_c1_sfgaex2_chunksizes[3] = {1, 96, 144} ;
static size_t num_c2_chunksizes[4] = {1, 30, 96, 144} ;
static size_t num_c2DDF_chunksizes[3] = {1, 96, 144} ;
static size_t num_c2GVF_chunksizes[3] = {1, 96, 144} ;
static size_t num_c2SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t num_c2SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t num_c2SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t num_c2SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t num_c2SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t num_c2TBF_chunksizes[3] = {1, 96, 144} ;
static size_t num_c2_sfcsiz1_chunksizes[3] = {1, 96, 144} ;
static size_t num_c2_sfcsiz2_chunksizes[3] = {1, 96, 144} ;
static size_t num_c2_sfcsiz3_chunksizes[3] = {1, 96, 144} ;
static size_t num_c2_sfcsiz4_chunksizes[3] = {1, 96, 144} ;
static size_t num_c2_sfgaex2_chunksizes[3] = {1, 96, 144} ;
static size_t num_c3_chunksizes[4] = {1, 30, 96, 144} ;
static size_t num_c3DDF_chunksizes[3] = {1, 96, 144} ;
static size_t num_c3GVF_chunksizes[3] = {1, 96, 144} ;
static size_t num_c3SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t num_c3SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t num_c3SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t num_c3SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t num_c3SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t num_c3TBF_chunksizes[3] = {1, 96, 144} ;
static size_t num_c3_sfcsiz1_chunksizes[3] = {1, 96, 144} ;
static size_t num_c3_sfcsiz2_chunksizes[3] = {1, 96, 144} ;
static size_t pom_a1_chunksizes[4] = {1, 30, 96, 144} ;
static size_t pom_a1DDF_chunksizes[3] = {1, 96, 144} ;
static size_t pom_a1GVF_chunksizes[3] = {1, 96, 144} ;
static size_t pom_a1SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t pom_a1SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t pom_a1SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t pom_a1SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t pom_a1SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t pom_a1TBF_chunksizes[3] = {1, 96, 144} ;
static size_t pom_a1_CLXF_chunksizes[3] = {1, 96, 144} ;
static size_t pom_a1_XFRC_chunksizes[4] = {1, 30, 96, 144} ;
static size_t pom_c1_chunksizes[4] = {1, 30, 96, 144} ;
static size_t pom_c1DDF_chunksizes[3] = {1, 96, 144} ;
static size_t pom_c1GVF_chunksizes[3] = {1, 96, 144} ;
static size_t pom_c1SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t pom_c1SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t pom_c1SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t pom_c1SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t pom_c1SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t pom_c1TBF_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a1_chunksizes[4] = {1, 30, 96, 144} ;
static size_t so4_a1DDF_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a1GVF_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a1SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a1SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a1SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a1SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a1SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a1TBF_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a1_CLXF_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a1_XFRC_chunksizes[4] = {1, 30, 96, 144} ;
static size_t so4_a1_sfcoag1_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a1_sfcsiz3_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a1_sfcsiz4_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a1_sfgaex1_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a1_sfgaex2_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a2_chunksizes[4] = {1, 30, 96, 144} ;
static size_t so4_a2DDF_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a2GVF_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a2SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a2SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a2SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a2SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a2SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a2TBF_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a2_CLXF_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a2_XFRC_chunksizes[4] = {1, 30, 96, 144} ;
static size_t so4_a2_sfcoag1_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a2_sfcsiz3_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a2_sfcsiz4_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a2_sfgaex1_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a2_sfgaex2_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a2_sfnnuc1_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a3_chunksizes[4] = {1, 30, 96, 144} ;
static size_t so4_a3DDF_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a3GVF_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a3SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a3SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a3SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a3SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a3SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a3TBF_chunksizes[3] = {1, 96, 144} ;
static size_t so4_a3_sfgaex1_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c1_chunksizes[4] = {1, 30, 96, 144} ;
static size_t so4_c1AQH2SO4_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c1AQSO4_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c1DDF_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c1GVF_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c1SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c1SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c1SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c1SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c1SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c1TBF_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c1_sfcsiz3_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c1_sfcsiz4_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c1_sfgaex2_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c2_chunksizes[4] = {1, 30, 96, 144} ;
static size_t so4_c2AQH2SO4_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c2AQSO4_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c2DDF_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c2GVF_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c2SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c2SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c2SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c2SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c2SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c2TBF_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c2_sfcsiz3_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c2_sfcsiz4_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c2_sfgaex2_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c3_chunksizes[4] = {1, 30, 96, 144} ;
static size_t so4_c3AQH2SO4_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c3AQSO4_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c3DDF_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c3GVF_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c3SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c3SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c3SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c3SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c3SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t so4_c3TBF_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a1_chunksizes[4] = {1, 30, 96, 144} ;
static size_t soa_a1DDF_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a1GVF_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a1SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a1SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a1SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a1SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a1SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a1TBF_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a1_sfcoag1_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a1_sfcsiz3_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a1_sfcsiz4_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a1_sfgaex1_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a1_sfgaex2_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a2_chunksizes[4] = {1, 30, 96, 144} ;
static size_t soa_a2DDF_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a2GVF_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a2SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a2SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a2SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a2SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a2SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a2TBF_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a2_sfcoag1_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a2_sfcsiz3_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a2_sfcsiz4_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a2_sfgaex1_chunksizes[3] = {1, 96, 144} ;
static size_t soa_a2_sfgaex2_chunksizes[3] = {1, 96, 144} ;
static size_t soa_c1_chunksizes[4] = {1, 30, 96, 144} ;
static size_t soa_c1DDF_chunksizes[3] = {1, 96, 144} ;
static size_t soa_c1GVF_chunksizes[3] = {1, 96, 144} ;
static size_t soa_c1SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t soa_c1SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t soa_c1SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t soa_c1SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t soa_c1SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t soa_c1TBF_chunksizes[3] = {1, 96, 144} ;
static size_t soa_c1_sfcsiz3_chunksizes[3] = {1, 96, 144} ;
static size_t soa_c1_sfcsiz4_chunksizes[3] = {1, 96, 144} ;
static size_t soa_c1_sfgaex2_chunksizes[3] = {1, 96, 144} ;
static size_t soa_c2_chunksizes[4] = {1, 30, 96, 144} ;
static size_t soa_c2DDF_chunksizes[3] = {1, 96, 144} ;
static size_t soa_c2GVF_chunksizes[3] = {1, 96, 144} ;
static size_t soa_c2SFSBC_chunksizes[3] = {1, 96, 144} ;
static size_t soa_c2SFSBS_chunksizes[3] = {1, 96, 144} ;
static size_t soa_c2SFSIC_chunksizes[3] = {1, 96, 144} ;
static size_t soa_c2SFSIS_chunksizes[3] = {1, 96, 144} ;
static size_t soa_c2SFWET_chunksizes[3] = {1, 96, 144} ;
static size_t soa_c2TBF_chunksizes[3] = {1, 96, 144} ;
static size_t soa_c2_sfcsiz3_chunksizes[3] = {1, 96, 144} ;
static size_t soa_c2_sfcsiz4_chunksizes[3] = {1, 96, 144} ;
static size_t soa_c2_sfgaex2_chunksizes[3] = {1, 96, 144} ;
static size_t wat_a1_chunksizes[4] = {1, 30, 96, 144} ;
static size_t wat_a2_chunksizes[4] = {1, 30, 96, 144} ;
static size_t wat_a3_chunksizes[4] = {1, 30, 96, 144} ;

void
check_err(const int stat, const int line, const char *file) {
    if (stat != NC_NOERR) {
        (void)fprintf(stderr,"line %d of %s: %s\n", line, file, nc_strerror(stat));
        fflush(stderr);
        exit(1);
    }
}

int
main() {/* create ref_camrun.nc */

    int  stat;  /* return status */
    int  ncid;  /* netCDF id */

    /* group ids */
    int camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp;

    /* dimension ids */
    int lat_dim;
    int lon_dim;
    int slat_dim;
    int slon_dim;
    int lev_dim;
    int ilev_dim;
    int isccp_prs_dim;
    int isccp_tau_dim;
    int isccp_prstau_dim;
    int time_dim;
    int tbnd_dim;
    int chars_dim;

    /* dimension lengths */
    size_t lat_len = 96;
    size_t lon_len = 144;
    size_t slat_len = 95;
    size_t slon_len = 144;
    size_t lev_len = 30;
    size_t ilev_len = 31;
    size_t isccp_prs_len = 7;
    size_t isccp_tau_len = 7;
    size_t isccp_prstau_len = 49;
    size_t time_len = NC_UNLIMITED;
    size_t tbnd_len = 2;
    size_t chars_len = 8;

    /* variable ids */
    int P0_id;
    int lat_id;
    int lon_id;
    int slat_id;
    int slon_id;
    int w_stag_id;
    int lev_id;
    int ilev_id;
    int isccp_prs_id;
    int isccp_tau_id;
    int isccp_prstau_id;
    int time_id;
    int time_bnds_id;
    int date_written_id;
    int time_written_id;
    int ntrm_id;
    int ntrn_id;
    int ntrk_id;
    int ndbase_id;
    int nsbase_id;
    int nbdate_id;
    int nbsec_id;
    int mdt_id;
    int nlon_id;
    int wnummax_id;
    int hyai_id;
    int hybi_id;
    int hyam_id;
    int hybm_id;
    int gw_id;
    int ndcur_id;
    int nscur_id;
    int date_id;
    int co2vmr_id;
    int ch4vmr_id;
    int n2ovmr_id;
    int f11vmr_id;
    int f12vmr_id;
    int sol_tsi_id;
    int datesec_id;
    int nsteph_id;
    int ABSORB_id;
    int AEROD_v_id;
    int AODABS_id;
    int AODDUST1_id;
    int AODDUST2_id;
    int AODDUST3_id;
    int AODMODE1_id;
    int AODMODE2_id;
    int AODMODE3_id;
    int AODVIS_id;
    int AQSO4_H2O2_id;
    int AQSO4_O3_id;
    int AQ_DMS_id;
    int AQ_H2O2_id;
    int AQ_H2SO4_id;
    int AQ_SO2_id;
    int AQ_SOAG_id;
    int AQ_bc_a1_id;
    int AQ_dst_a1_id;
    int AQ_dst_a3_id;
    int AQ_ncl_a1_id;
    int AQ_ncl_a2_id;
    int AQ_ncl_a3_id;
    int AQ_num_a1_id;
    int AQ_num_a2_id;
    int AQ_num_a3_id;
    int AQ_pom_a1_id;
    int AQ_so4_a1_id;
    int AQ_so4_a2_id;
    int AQ_so4_a3_id;
    int AQ_soa_a1_id;
    int AQ_soa_a2_id;
    int BPROD_id;
    int BURDEN1_id;
    int BURDEN2_id;
    int BURDEN3_id;
    int CCN1_id;
    int CCN2_id;
    int CCN3_id;
    int CCN4_id;
    int CCN5_id;
    int CCN6_id;
    int CLDHGH_id;
    int CLDICE_id;
    int CLDLIQ_id;
    int CLDLOW_id;
    int CLDMED_id;
    int CLDTOT_id;
    int CLOUD_id;
    int CMFDQ_id;
    int CMFDQR_id;
    int CMFDT_id;
    int CMFMC_id;
    int CMFMCDZM_id;
    int CONCLD_id;
    int DCQ_id;
    int DMS_id;
    int DSTODXC_id;
    int DSTSFDRY_id;
    int DSTSFMBL_id;
    int DSTSFWET_id;
    int DTCOND_id;
    int DTV_id;
    int EXTINCT_id;
    int FICE_id;
    int FLDS_id;
    int FLNS_id;
    int FLNSC_id;
    int FLNT_id;
    int FLNTC_id;
    int FLUT_id;
    int FLUTC_id;
    int FREQSH_id;
    int FREQZM_id;
    int FSDS_id;
    int FSDSC_id;
    int FSNS_id;
    int FSNSC_id;
    int FSNT_id;
    int FSNTC_id;
    int FSNTOA_id;
    int FSNTOAC_id;
    int FSUTOA_id;
    int GS_DMS_id;
    int GS_H2O2_id;
    int GS_H2SO4_id;
    int GS_SO2_id;
    int GS_SOAG_id;
    int GS_bc_a1_id;
    int GS_dst_a1_id;
    int GS_dst_a3_id;
    int GS_ncl_a1_id;
    int GS_ncl_a2_id;
    int GS_ncl_a3_id;
    int GS_num_a1_id;
    int GS_num_a2_id;
    int GS_num_a3_id;
    int GS_pom_a1_id;
    int GS_so4_a1_id;
    int GS_so4_a2_id;
    int GS_so4_a3_id;
    int GS_soa_a1_id;
    int GS_soa_a2_id;
    int H2O2_id;
    int H2SO4_id;
    int H2SO4_sfgaex1_id;
    int H2SO4_sfnnuc1_id;
    int ICEFRAC_id;
    int ICIMR_id;
    int ICWMR_id;
    int KVH_id;
    int KVM_id;
    int LANDFRAC_id;
    int LCLOUD_id;
    int LHFLX_id;
    int LND_MBL_id;
    int LWCF_id;
    int NDROPCOL_id;
    int NDROPMIX_id;
    int NDROPSNK_id;
    int NDROPSRC_id;
    int NUMICE_id;
    int NUMLIQ_id;
    int OCNFRAC_id;
    int ODV_bc_a1_id;
    int ODV_dst_a1_id;
    int ODV_dst_a3_id;
    int ODV_ncl_a1_id;
    int ODV_ncl_a3_id;
    int ODV_pom_a1_id;
    int ODV_so4_a1_id;
    int ODV_soa_a1_id;
    int OMEGA_id;
    int OMEGAT_id;
    int ORO_id;
    int PBLH_id;
    int PCONVB_id;
    int PCONVT_id;
    int PHIS_id;
    int PRECC_id;
    int PRECCDZM_id;
    int PRECL_id;
    int PRECSC_id;
    int PRECSH_id;
    int PRECSL_id;
    int PRECT_id;
    int PS_id;
    int PSL_id;
    int Q_id;
    int QC_id;
    int QFLX_id;
    int QREFHT_id;
    int QRL_id;
    int QRS_id;
    int QT_id;
    int QTFLX_id;
    int RAM1_id;
    int RELHUM_id;
    int RHREFHT_id;
    int SFCLDICE_id;
    int SFCLDLIQ_id;
    int SFI_id;
    int SFNUMICE_id;
    int SFNUMLIQ_id;
    int SHFLX_id;
    int SL_id;
    int SLFLX_id;
    int SLV_id;
    int SNOWHICE_id;
    int SNOWHLND_id;
    int SO2_id;
    int SO2_CLXF_id;
    int SO2_XFRC_id;
    int SOAG_id;
    int SOAG_sfgaex1_id;
    int SOLIN_id;
    int SPROD_id;
    int SRFRAD_id;
    int SSAVIS_id;
    int SSTODXC_id;
    int SSTSFDRY_id;
    int SSTSFMBL_id;
    int SSTSFWET_id;
    int SWCF_id;
    int T_id;
    int TAUTMSX_id;
    int TAUTMSY_id;
    int TAUX_id;
    int TAUY_id;
    int TGCLDCWP_id;
    int TGCLDIWP_id;
    int TGCLDLWP_id;
    int TKE_id;
    int TMQ_id;
    int TREFHT_id;
    int TREFMNAV_id;
    int TREFMXAV_id;
    int TROP_FD_id;
    int TROP_P_id;
    int TROP_PD_id;
    int TROP_T_id;
    int TROP_Z_id;
    int TS_id;
    int TSMN_id;
    int TSMX_id;
    int U_id;
    int UFLX_id;
    int US_id;
    int UU_id;
    int V_id;
    int VD01_id;
    int VFLX_id;
    int VQ_id;
    int VS_id;
    int VT_id;
    int VU_id;
    int VV_id;
    int WGUSTD_id;
    int WTKE_id;
    int XPH_LWC_id;
    int Z3_id;
    int airFV_id;
    int bc_a1_id;
    int bc_a1DDF_id;
    int bc_a1GVF_id;
    int bc_a1SFSBC_id;
    int bc_a1SFSBS_id;
    int bc_a1SFSIC_id;
    int bc_a1SFSIS_id;
    int bc_a1SFWET_id;
    int bc_a1TBF_id;
    int bc_a1_CLXF_id;
    int bc_a1_XFRC_id;
    int bc_c1_id;
    int bc_c1DDF_id;
    int bc_c1GVF_id;
    int bc_c1SFSBC_id;
    int bc_c1SFSBS_id;
    int bc_c1SFSIC_id;
    int bc_c1SFSIS_id;
    int bc_c1SFWET_id;
    int bc_c1TBF_id;
    int chem_trop_id;
    int chem_trop_tropop_id;
    int dgnd_a01_id;
    int dgnd_a02_id;
    int dgnd_a03_id;
    int dgnw_a01_id;
    int dgnw_a02_id;
    int dgnw_a03_id;
    int dst_a1_id;
    int dst_a1DDF_id;
    int dst_a1GVF_id;
    int dst_a1SF_id;
    int dst_a1SFSBC_id;
    int dst_a1SFSBS_id;
    int dst_a1SFSIC_id;
    int dst_a1SFSIS_id;
    int dst_a1SFWET_id;
    int dst_a1TBF_id;
    int dst_a3_id;
    int dst_a3DDF_id;
    int dst_a3GVF_id;
    int dst_a3SF_id;
    int dst_a3SFSBC_id;
    int dst_a3SFSBS_id;
    int dst_a3SFSIC_id;
    int dst_a3SFSIS_id;
    int dst_a3SFWET_id;
    int dst_a3TBF_id;
    int dst_c1_id;
    int dst_c1DDF_id;
    int dst_c1GVF_id;
    int dst_c1SFSBC_id;
    int dst_c1SFSBS_id;
    int dst_c1SFSIC_id;
    int dst_c1SFSIS_id;
    int dst_c1SFWET_id;
    int dst_c1TBF_id;
    int dst_c3_id;
    int dst_c3DDF_id;
    int dst_c3GVF_id;
    int dst_c3SFSBC_id;
    int dst_c3SFSBS_id;
    int dst_c3SFSIC_id;
    int dst_c3SFSIS_id;
    int dst_c3SFWET_id;
    int dst_c3TBF_id;
    int ncl_a1_id;
    int ncl_a1DDF_id;
    int ncl_a1GVF_id;
    int ncl_a1SF_id;
    int ncl_a1SFSBC_id;
    int ncl_a1SFSBS_id;
    int ncl_a1SFSIC_id;
    int ncl_a1SFSIS_id;
    int ncl_a1SFWET_id;
    int ncl_a1TBF_id;
    int ncl_a1_sfcoag1_id;
    int ncl_a1_sfcsiz3_id;
    int ncl_a1_sfcsiz4_id;
    int ncl_a1_sfgaex2_id;
    int ncl_a2_id;
    int ncl_a2DDF_id;
    int ncl_a2GVF_id;
    int ncl_a2SF_id;
    int ncl_a2SFSBC_id;
    int ncl_a2SFSBS_id;
    int ncl_a2SFSIC_id;
    int ncl_a2SFSIS_id;
    int ncl_a2SFWET_id;
    int ncl_a2TBF_id;
    int ncl_a2_sfcoag1_id;
    int ncl_a2_sfcsiz3_id;
    int ncl_a2_sfcsiz4_id;
    int ncl_a2_sfgaex2_id;
    int ncl_a3_id;
    int ncl_a3DDF_id;
    int ncl_a3GVF_id;
    int ncl_a3SF_id;
    int ncl_a3SFSBC_id;
    int ncl_a3SFSBS_id;
    int ncl_a3SFSIC_id;
    int ncl_a3SFSIS_id;
    int ncl_a3SFWET_id;
    int ncl_a3TBF_id;
    int ncl_c1_id;
    int ncl_c1DDF_id;
    int ncl_c1GVF_id;
    int ncl_c1SFSBC_id;
    int ncl_c1SFSBS_id;
    int ncl_c1SFSIC_id;
    int ncl_c1SFSIS_id;
    int ncl_c1SFWET_id;
    int ncl_c1TBF_id;
    int ncl_c1_sfcsiz3_id;
    int ncl_c1_sfcsiz4_id;
    int ncl_c1_sfgaex2_id;
    int ncl_c2_id;
    int ncl_c2DDF_id;
    int ncl_c2GVF_id;
    int ncl_c2SFSBC_id;
    int ncl_c2SFSBS_id;
    int ncl_c2SFSIC_id;
    int ncl_c2SFSIS_id;
    int ncl_c2SFWET_id;
    int ncl_c2TBF_id;
    int ncl_c2_sfcsiz3_id;
    int ncl_c2_sfcsiz4_id;
    int ncl_c2_sfgaex2_id;
    int ncl_c3_id;
    int ncl_c3DDF_id;
    int ncl_c3GVF_id;
    int ncl_c3SFSBC_id;
    int ncl_c3SFSBS_id;
    int ncl_c3SFSIC_id;
    int ncl_c3SFSIS_id;
    int ncl_c3SFWET_id;
    int ncl_c3TBF_id;
    int num_a1_id;
    int num_a1DDF_id;
    int num_a1GVF_id;
    int num_a1SFSBC_id;
    int num_a1SFSBS_id;
    int num_a1SFSIC_id;
    int num_a1SFSIS_id;
    int num_a1SFWET_id;
    int num_a1TBF_id;
    int num_a1_CLXF_id;
    int num_a1_XFRC_id;
    int num_a1_sfcoag1_id;
    int num_a1_sfcsiz1_id;
    int num_a1_sfcsiz2_id;
    int num_a1_sfcsiz3_id;
    int num_a1_sfcsiz4_id;
    int num_a1_sfgaex2_id;
    int num_a2_id;
    int num_a2DDF_id;
    int num_a2GVF_id;
    int num_a2SFSBC_id;
    int num_a2SFSBS_id;
    int num_a2SFSIC_id;
    int num_a2SFSIS_id;
    int num_a2SFWET_id;
    int num_a2TBF_id;
    int num_a2_CLXF_id;
    int num_a2_XFRC_id;
    int num_a2_sfcoag1_id;
    int num_a2_sfcsiz1_id;
    int num_a2_sfcsiz2_id;
    int num_a2_sfcsiz3_id;
    int num_a2_sfcsiz4_id;
    int num_a2_sfgaex2_id;
    int num_a2_sfnnuc1_id;
    int num_a3_id;
    int num_a3DDF_id;
    int num_a3GVF_id;
    int num_a3SFSBC_id;
    int num_a3SFSBS_id;
    int num_a3SFSIC_id;
    int num_a3SFSIS_id;
    int num_a3SFWET_id;
    int num_a3TBF_id;
    int num_a3_sfcsiz1_id;
    int num_a3_sfcsiz2_id;
    int num_c1_id;
    int num_c1DDF_id;
    int num_c1GVF_id;
    int num_c1SFSBC_id;
    int num_c1SFSBS_id;
    int num_c1SFSIC_id;
    int num_c1SFSIS_id;
    int num_c1SFWET_id;
    int num_c1TBF_id;
    int num_c1_sfcsiz1_id;
    int num_c1_sfcsiz2_id;
    int num_c1_sfcsiz3_id;
    int num_c1_sfcsiz4_id;
    int num_c1_sfgaex2_id;
    int num_c2_id;
    int num_c2DDF_id;
    int num_c2GVF_id;
    int num_c2SFSBC_id;
    int num_c2SFSBS_id;
    int num_c2SFSIC_id;
    int num_c2SFSIS_id;
    int num_c2SFWET_id;
    int num_c2TBF_id;
    int num_c2_sfcsiz1_id;
    int num_c2_sfcsiz2_id;
    int num_c2_sfcsiz3_id;
    int num_c2_sfcsiz4_id;
    int num_c2_sfgaex2_id;
    int num_c3_id;
    int num_c3DDF_id;
    int num_c3GVF_id;
    int num_c3SFSBC_id;
    int num_c3SFSBS_id;
    int num_c3SFSIC_id;
    int num_c3SFSIS_id;
    int num_c3SFWET_id;
    int num_c3TBF_id;
    int num_c3_sfcsiz1_id;
    int num_c3_sfcsiz2_id;
    int pom_a1_id;
    int pom_a1DDF_id;
    int pom_a1GVF_id;
    int pom_a1SFSBC_id;
    int pom_a1SFSBS_id;
    int pom_a1SFSIC_id;
    int pom_a1SFSIS_id;
    int pom_a1SFWET_id;
    int pom_a1TBF_id;
    int pom_a1_CLXF_id;
    int pom_a1_XFRC_id;
    int pom_c1_id;
    int pom_c1DDF_id;
    int pom_c1GVF_id;
    int pom_c1SFSBC_id;
    int pom_c1SFSBS_id;
    int pom_c1SFSIC_id;
    int pom_c1SFSIS_id;
    int pom_c1SFWET_id;
    int pom_c1TBF_id;
    int so4_a1_id;
    int so4_a1DDF_id;
    int so4_a1GVF_id;
    int so4_a1SFSBC_id;
    int so4_a1SFSBS_id;
    int so4_a1SFSIC_id;
    int so4_a1SFSIS_id;
    int so4_a1SFWET_id;
    int so4_a1TBF_id;
    int so4_a1_CLXF_id;
    int so4_a1_XFRC_id;
    int so4_a1_sfcoag1_id;
    int so4_a1_sfcsiz3_id;
    int so4_a1_sfcsiz4_id;
    int so4_a1_sfgaex1_id;
    int so4_a1_sfgaex2_id;
    int so4_a2_id;
    int so4_a2DDF_id;
    int so4_a2GVF_id;
    int so4_a2SFSBC_id;
    int so4_a2SFSBS_id;
    int so4_a2SFSIC_id;
    int so4_a2SFSIS_id;
    int so4_a2SFWET_id;
    int so4_a2TBF_id;
    int so4_a2_CLXF_id;
    int so4_a2_XFRC_id;
    int so4_a2_sfcoag1_id;
    int so4_a2_sfcsiz3_id;
    int so4_a2_sfcsiz4_id;
    int so4_a2_sfgaex1_id;
    int so4_a2_sfgaex2_id;
    int so4_a2_sfnnuc1_id;
    int so4_a3_id;
    int so4_a3DDF_id;
    int so4_a3GVF_id;
    int so4_a3SFSBC_id;
    int so4_a3SFSBS_id;
    int so4_a3SFSIC_id;
    int so4_a3SFSIS_id;
    int so4_a3SFWET_id;
    int so4_a3TBF_id;
    int so4_a3_sfgaex1_id;
    int so4_c1_id;
    int so4_c1AQH2SO4_id;
    int so4_c1AQSO4_id;
    int so4_c1DDF_id;
    int so4_c1GVF_id;
    int so4_c1SFSBC_id;
    int so4_c1SFSBS_id;
    int so4_c1SFSIC_id;
    int so4_c1SFSIS_id;
    int so4_c1SFWET_id;
    int so4_c1TBF_id;
    int so4_c1_sfcsiz3_id;
    int so4_c1_sfcsiz4_id;
    int so4_c1_sfgaex2_id;
    int so4_c2_id;
    int so4_c2AQH2SO4_id;
    int so4_c2AQSO4_id;
    int so4_c2DDF_id;
    int so4_c2GVF_id;
    int so4_c2SFSBC_id;
    int so4_c2SFSBS_id;
    int so4_c2SFSIC_id;
    int so4_c2SFSIS_id;
    int so4_c2SFWET_id;
    int so4_c2TBF_id;
    int so4_c2_sfcsiz3_id;
    int so4_c2_sfcsiz4_id;
    int so4_c2_sfgaex2_id;
    int so4_c3_id;
    int so4_c3AQH2SO4_id;
    int so4_c3AQSO4_id;
    int so4_c3DDF_id;
    int so4_c3GVF_id;
    int so4_c3SFSBC_id;
    int so4_c3SFSBS_id;
    int so4_c3SFSIC_id;
    int so4_c3SFSIS_id;
    int so4_c3SFWET_id;
    int so4_c3TBF_id;
    int soa_a1_id;
    int soa_a1DDF_id;
    int soa_a1GVF_id;
    int soa_a1SFSBC_id;
    int soa_a1SFSBS_id;
    int soa_a1SFSIC_id;
    int soa_a1SFSIS_id;
    int soa_a1SFWET_id;
    int soa_a1TBF_id;
    int soa_a1_sfcoag1_id;
    int soa_a1_sfcsiz3_id;
    int soa_a1_sfcsiz4_id;
    int soa_a1_sfgaex1_id;
    int soa_a1_sfgaex2_id;
    int soa_a2_id;
    int soa_a2DDF_id;
    int soa_a2GVF_id;
    int soa_a2SFSBC_id;
    int soa_a2SFSBS_id;
    int soa_a2SFSIC_id;
    int soa_a2SFSIS_id;
    int soa_a2SFWET_id;
    int soa_a2TBF_id;
    int soa_a2_sfcoag1_id;
    int soa_a2_sfcsiz3_id;
    int soa_a2_sfcsiz4_id;
    int soa_a2_sfgaex1_id;
    int soa_a2_sfgaex2_id;
    int soa_c1_id;
    int soa_c1DDF_id;
    int soa_c1GVF_id;
    int soa_c1SFSBC_id;
    int soa_c1SFSBS_id;
    int soa_c1SFSIC_id;
    int soa_c1SFSIS_id;
    int soa_c1SFWET_id;
    int soa_c1TBF_id;
    int soa_c1_sfcsiz3_id;
    int soa_c1_sfcsiz4_id;
    int soa_c1_sfgaex2_id;
    int soa_c2_id;
    int soa_c2DDF_id;
    int soa_c2GVF_id;
    int soa_c2SFSBC_id;
    int soa_c2SFSBS_id;
    int soa_c2SFSIC_id;
    int soa_c2SFSIS_id;
    int soa_c2SFWET_id;
    int soa_c2TBF_id;
    int soa_c2_sfcsiz3_id;
    int soa_c2_sfcsiz4_id;
    int soa_c2_sfgaex2_id;
    int wat_a1_id;
    int wat_a2_id;
    int wat_a3_id;

    /* rank (number of dimensions) for each variable */
#   define RANK_P0 0
#   define RANK_lat 1
#   define RANK_lon 1
#   define RANK_slat 1
#   define RANK_slon 1
#   define RANK_w_stag 1
#   define RANK_lev 1
#   define RANK_ilev 1
#   define RANK_isccp_prs 1
#   define RANK_isccp_tau 1
#   define RANK_isccp_prstau 1
#   define RANK_time 1
#   define RANK_time_bnds 2
#   define RANK_date_written 2
#   define RANK_time_written 2
#   define RANK_ntrm 0
#   define RANK_ntrn 0
#   define RANK_ntrk 0
#   define RANK_ndbase 0
#   define RANK_nsbase 0
#   define RANK_nbdate 0
#   define RANK_nbsec 0
#   define RANK_mdt 0
#   define RANK_nlon 1
#   define RANK_wnummax 1
#   define RANK_hyai 1
#   define RANK_hybi 1
#   define RANK_hyam 1
#   define RANK_hybm 1
#   define RANK_gw 1
#   define RANK_ndcur 1
#   define RANK_nscur 1
#   define RANK_date 1
#   define RANK_co2vmr 1
#   define RANK_ch4vmr 1
#   define RANK_n2ovmr 1
#   define RANK_f11vmr 1
#   define RANK_f12vmr 1
#   define RANK_sol_tsi 1
#   define RANK_datesec 1
#   define RANK_nsteph 1
#   define RANK_ABSORB 4
#   define RANK_AEROD_v 3
#   define RANK_AODABS 3
#   define RANK_AODDUST1 3
#   define RANK_AODDUST2 3
#   define RANK_AODDUST3 3
#   define RANK_AODMODE1 3
#   define RANK_AODMODE2 3
#   define RANK_AODMODE3 3
#   define RANK_AODVIS 3
#   define RANK_AQSO4_H2O2 3
#   define RANK_AQSO4_O3 3
#   define RANK_AQ_DMS 3
#   define RANK_AQ_H2O2 3
#   define RANK_AQ_H2SO4 3
#   define RANK_AQ_SO2 3
#   define RANK_AQ_SOAG 3
#   define RANK_AQ_bc_a1 3
#   define RANK_AQ_dst_a1 3
#   define RANK_AQ_dst_a3 3
#   define RANK_AQ_ncl_a1 3
#   define RANK_AQ_ncl_a2 3
#   define RANK_AQ_ncl_a3 3
#   define RANK_AQ_num_a1 3
#   define RANK_AQ_num_a2 3
#   define RANK_AQ_num_a3 3
#   define RANK_AQ_pom_a1 3
#   define RANK_AQ_so4_a1 3
#   define RANK_AQ_so4_a2 3
#   define RANK_AQ_so4_a3 3
#   define RANK_AQ_soa_a1 3
#   define RANK_AQ_soa_a2 3
#   define RANK_BPROD 4
#   define RANK_BURDEN1 3
#   define RANK_BURDEN2 3
#   define RANK_BURDEN3 3
#   define RANK_CCN1 4
#   define RANK_CCN2 4
#   define RANK_CCN3 4
#   define RANK_CCN4 4
#   define RANK_CCN5 4
#   define RANK_CCN6 4
#   define RANK_CLDHGH 3
#   define RANK_CLDICE 4
#   define RANK_CLDLIQ 4
#   define RANK_CLDLOW 3
#   define RANK_CLDMED 3
#   define RANK_CLDTOT 3
#   define RANK_CLOUD 4
#   define RANK_CMFDQ 4
#   define RANK_CMFDQR 4
#   define RANK_CMFDT 4
#   define RANK_CMFMC 4
#   define RANK_CMFMCDZM 4
#   define RANK_CONCLD 4
#   define RANK_DCQ 4
#   define RANK_DMS 4
#   define RANK_DSTODXC 3
#   define RANK_DSTSFDRY 3
#   define RANK_DSTSFMBL 3
#   define RANK_DSTSFWET 3
#   define RANK_DTCOND 4
#   define RANK_DTV 4
#   define RANK_EXTINCT 4
#   define RANK_FICE 4
#   define RANK_FLDS 3
#   define RANK_FLNS 3
#   define RANK_FLNSC 3
#   define RANK_FLNT 3
#   define RANK_FLNTC 3
#   define RANK_FLUT 3
#   define RANK_FLUTC 3
#   define RANK_FREQSH 3
#   define RANK_FREQZM 3
#   define RANK_FSDS 3
#   define RANK_FSDSC 3
#   define RANK_FSNS 3
#   define RANK_FSNSC 3
#   define RANK_FSNT 3
#   define RANK_FSNTC 3
#   define RANK_FSNTOA 3
#   define RANK_FSNTOAC 3
#   define RANK_FSUTOA 3
#   define RANK_GS_DMS 3
#   define RANK_GS_H2O2 3
#   define RANK_GS_H2SO4 3
#   define RANK_GS_SO2 3
#   define RANK_GS_SOAG 3
#   define RANK_GS_bc_a1 3
#   define RANK_GS_dst_a1 3
#   define RANK_GS_dst_a3 3
#   define RANK_GS_ncl_a1 3
#   define RANK_GS_ncl_a2 3
#   define RANK_GS_ncl_a3 3
#   define RANK_GS_num_a1 3
#   define RANK_GS_num_a2 3
#   define RANK_GS_num_a3 3
#   define RANK_GS_pom_a1 3
#   define RANK_GS_so4_a1 3
#   define RANK_GS_so4_a2 3
#   define RANK_GS_so4_a3 3
#   define RANK_GS_soa_a1 3
#   define RANK_GS_soa_a2 3
#   define RANK_H2O2 4
#   define RANK_H2SO4 4
#   define RANK_H2SO4_sfgaex1 3
#   define RANK_H2SO4_sfnnuc1 3
#   define RANK_ICEFRAC 3
#   define RANK_ICIMR 4
#   define RANK_ICWMR 4
#   define RANK_KVH 4
#   define RANK_KVM 4
#   define RANK_LANDFRAC 3
#   define RANK_LCLOUD 4
#   define RANK_LHFLX 3
#   define RANK_LND_MBL 3
#   define RANK_LWCF 3
#   define RANK_NDROPCOL 3
#   define RANK_NDROPMIX 4
#   define RANK_NDROPSNK 4
#   define RANK_NDROPSRC 4
#   define RANK_NUMICE 4
#   define RANK_NUMLIQ 4
#   define RANK_OCNFRAC 3
#   define RANK_ODV_bc_a1 3
#   define RANK_ODV_dst_a1 3
#   define RANK_ODV_dst_a3 3
#   define RANK_ODV_ncl_a1 3
#   define RANK_ODV_ncl_a3 3
#   define RANK_ODV_pom_a1 3
#   define RANK_ODV_so4_a1 3
#   define RANK_ODV_soa_a1 3
#   define RANK_OMEGA 4
#   define RANK_OMEGAT 4
#   define RANK_ORO 3
#   define RANK_PBLH 3
#   define RANK_PCONVB 3
#   define RANK_PCONVT 3
#   define RANK_PHIS 3
#   define RANK_PRECC 3
#   define RANK_PRECCDZM 3
#   define RANK_PRECL 3
#   define RANK_PRECSC 3
#   define RANK_PRECSH 3
#   define RANK_PRECSL 3
#   define RANK_PRECT 3
#   define RANK_PS 3
#   define RANK_PSL 3
#   define RANK_Q 4
#   define RANK_QC 4
#   define RANK_QFLX 3
#   define RANK_QREFHT 3
#   define RANK_QRL 4
#   define RANK_QRS 4
#   define RANK_QT 4
#   define RANK_QTFLX 4
#   define RANK_RAM1 3
#   define RANK_RELHUM 4
#   define RANK_RHREFHT 3
#   define RANK_SFCLDICE 3
#   define RANK_SFCLDLIQ 3
#   define RANK_SFI 4
#   define RANK_SFNUMICE 3
#   define RANK_SFNUMLIQ 3
#   define RANK_SHFLX 3
#   define RANK_SL 4
#   define RANK_SLFLX 4
#   define RANK_SLV 4
#   define RANK_SNOWHICE 3
#   define RANK_SNOWHLND 3
#   define RANK_SO2 4
#   define RANK_SO2_CLXF 3
#   define RANK_SO2_XFRC 4
#   define RANK_SOAG 4
#   define RANK_SOAG_sfgaex1 3
#   define RANK_SOLIN 3
#   define RANK_SPROD 4
#   define RANK_SRFRAD 3
#   define RANK_SSAVIS 3
#   define RANK_SSTODXC 3
#   define RANK_SSTSFDRY 3
#   define RANK_SSTSFMBL 3
#   define RANK_SSTSFWET 3
#   define RANK_SWCF 3
#   define RANK_T 4
#   define RANK_TAUTMSX 3
#   define RANK_TAUTMSY 3
#   define RANK_TAUX 3
#   define RANK_TAUY 3
#   define RANK_TGCLDCWP 3
#   define RANK_TGCLDIWP 3
#   define RANK_TGCLDLWP 3
#   define RANK_TKE 4
#   define RANK_TMQ 3
#   define RANK_TREFHT 3
#   define RANK_TREFMNAV 3
#   define RANK_TREFMXAV 3
#   define RANK_TROP_FD 3
#   define RANK_TROP_P 3
#   define RANK_TROP_PD 4
#   define RANK_TROP_T 3
#   define RANK_TROP_Z 3
#   define RANK_TS 3
#   define RANK_TSMN 3
#   define RANK_TSMX 3
#   define RANK_U 4
#   define RANK_UFLX 4
#   define RANK_US 4
#   define RANK_UU 4
#   define RANK_V 4
#   define RANK_VD01 4
#   define RANK_VFLX 4
#   define RANK_VQ 4
#   define RANK_VS 4
#   define RANK_VT 4
#   define RANK_VU 4
#   define RANK_VV 4
#   define RANK_WGUSTD 3
#   define RANK_WTKE 4
#   define RANK_XPH_LWC 4
#   define RANK_Z3 4
#   define RANK_airFV 3
#   define RANK_bc_a1 4
#   define RANK_bc_a1DDF 3
#   define RANK_bc_a1GVF 3
#   define RANK_bc_a1SFSBC 3
#   define RANK_bc_a1SFSBS 3
#   define RANK_bc_a1SFSIC 3
#   define RANK_bc_a1SFSIS 3
#   define RANK_bc_a1SFWET 3
#   define RANK_bc_a1TBF 3
#   define RANK_bc_a1_CLXF 3
#   define RANK_bc_a1_XFRC 4
#   define RANK_bc_c1 4
#   define RANK_bc_c1DDF 3
#   define RANK_bc_c1GVF 3
#   define RANK_bc_c1SFSBC 3
#   define RANK_bc_c1SFSBS 3
#   define RANK_bc_c1SFSIC 3
#   define RANK_bc_c1SFSIS 3
#   define RANK_bc_c1SFWET 3
#   define RANK_bc_c1TBF 3
#   define RANK_chem_trop 4
#   define RANK_chem_trop_tropop 4
#   define RANK_dgnd_a01 4
#   define RANK_dgnd_a02 4
#   define RANK_dgnd_a03 4
#   define RANK_dgnw_a01 4
#   define RANK_dgnw_a02 4
#   define RANK_dgnw_a03 4
#   define RANK_dst_a1 4
#   define RANK_dst_a1DDF 3
#   define RANK_dst_a1GVF 3
#   define RANK_dst_a1SF 3
#   define RANK_dst_a1SFSBC 3
#   define RANK_dst_a1SFSBS 3
#   define RANK_dst_a1SFSIC 3
#   define RANK_dst_a1SFSIS 3
#   define RANK_dst_a1SFWET 3
#   define RANK_dst_a1TBF 3
#   define RANK_dst_a3 4
#   define RANK_dst_a3DDF 3
#   define RANK_dst_a3GVF 3
#   define RANK_dst_a3SF 3
#   define RANK_dst_a3SFSBC 3
#   define RANK_dst_a3SFSBS 3
#   define RANK_dst_a3SFSIC 3
#   define RANK_dst_a3SFSIS 3
#   define RANK_dst_a3SFWET 3
#   define RANK_dst_a3TBF 3
#   define RANK_dst_c1 4
#   define RANK_dst_c1DDF 3
#   define RANK_dst_c1GVF 3
#   define RANK_dst_c1SFSBC 3
#   define RANK_dst_c1SFSBS 3
#   define RANK_dst_c1SFSIC 3
#   define RANK_dst_c1SFSIS 3
#   define RANK_dst_c1SFWET 3
#   define RANK_dst_c1TBF 3
#   define RANK_dst_c3 4
#   define RANK_dst_c3DDF 3
#   define RANK_dst_c3GVF 3
#   define RANK_dst_c3SFSBC 3
#   define RANK_dst_c3SFSBS 3
#   define RANK_dst_c3SFSIC 3
#   define RANK_dst_c3SFSIS 3
#   define RANK_dst_c3SFWET 3
#   define RANK_dst_c3TBF 3
#   define RANK_ncl_a1 4
#   define RANK_ncl_a1DDF 3
#   define RANK_ncl_a1GVF 3
#   define RANK_ncl_a1SF 3
#   define RANK_ncl_a1SFSBC 3
#   define RANK_ncl_a1SFSBS 3
#   define RANK_ncl_a1SFSIC 3
#   define RANK_ncl_a1SFSIS 3
#   define RANK_ncl_a1SFWET 3
#   define RANK_ncl_a1TBF 3
#   define RANK_ncl_a1_sfcoag1 3
#   define RANK_ncl_a1_sfcsiz3 3
#   define RANK_ncl_a1_sfcsiz4 3
#   define RANK_ncl_a1_sfgaex2 3
#   define RANK_ncl_a2 4
#   define RANK_ncl_a2DDF 3
#   define RANK_ncl_a2GVF 3
#   define RANK_ncl_a2SF 3
#   define RANK_ncl_a2SFSBC 3
#   define RANK_ncl_a2SFSBS 3
#   define RANK_ncl_a2SFSIC 3
#   define RANK_ncl_a2SFSIS 3
#   define RANK_ncl_a2SFWET 3
#   define RANK_ncl_a2TBF 3
#   define RANK_ncl_a2_sfcoag1 3
#   define RANK_ncl_a2_sfcsiz3 3
#   define RANK_ncl_a2_sfcsiz4 3
#   define RANK_ncl_a2_sfgaex2 3
#   define RANK_ncl_a3 4
#   define RANK_ncl_a3DDF 3
#   define RANK_ncl_a3GVF 3
#   define RANK_ncl_a3SF 3
#   define RANK_ncl_a3SFSBC 3
#   define RANK_ncl_a3SFSBS 3
#   define RANK_ncl_a3SFSIC 3
#   define RANK_ncl_a3SFSIS 3
#   define RANK_ncl_a3SFWET 3
#   define RANK_ncl_a3TBF 3
#   define RANK_ncl_c1 4
#   define RANK_ncl_c1DDF 3
#   define RANK_ncl_c1GVF 3
#   define RANK_ncl_c1SFSBC 3
#   define RANK_ncl_c1SFSBS 3
#   define RANK_ncl_c1SFSIC 3
#   define RANK_ncl_c1SFSIS 3
#   define RANK_ncl_c1SFWET 3
#   define RANK_ncl_c1TBF 3
#   define RANK_ncl_c1_sfcsiz3 3
#   define RANK_ncl_c1_sfcsiz4 3
#   define RANK_ncl_c1_sfgaex2 3
#   define RANK_ncl_c2 4
#   define RANK_ncl_c2DDF 3
#   define RANK_ncl_c2GVF 3
#   define RANK_ncl_c2SFSBC 3
#   define RANK_ncl_c2SFSBS 3
#   define RANK_ncl_c2SFSIC 3
#   define RANK_ncl_c2SFSIS 3
#   define RANK_ncl_c2SFWET 3
#   define RANK_ncl_c2TBF 3
#   define RANK_ncl_c2_sfcsiz3 3
#   define RANK_ncl_c2_sfcsiz4 3
#   define RANK_ncl_c2_sfgaex2 3
#   define RANK_ncl_c3 4
#   define RANK_ncl_c3DDF 3
#   define RANK_ncl_c3GVF 3
#   define RANK_ncl_c3SFSBC 3
#   define RANK_ncl_c3SFSBS 3
#   define RANK_ncl_c3SFSIC 3
#   define RANK_ncl_c3SFSIS 3
#   define RANK_ncl_c3SFWET 3
#   define RANK_ncl_c3TBF 3
#   define RANK_num_a1 4
#   define RANK_num_a1DDF 3
#   define RANK_num_a1GVF 3
#   define RANK_num_a1SFSBC 3
#   define RANK_num_a1SFSBS 3
#   define RANK_num_a1SFSIC 3
#   define RANK_num_a1SFSIS 3
#   define RANK_num_a1SFWET 3
#   define RANK_num_a1TBF 3
#   define RANK_num_a1_CLXF 3
#   define RANK_num_a1_XFRC 4
#   define RANK_num_a1_sfcoag1 3
#   define RANK_num_a1_sfcsiz1 3
#   define RANK_num_a1_sfcsiz2 3
#   define RANK_num_a1_sfcsiz3 3
#   define RANK_num_a1_sfcsiz4 3
#   define RANK_num_a1_sfgaex2 3
#   define RANK_num_a2 4
#   define RANK_num_a2DDF 3
#   define RANK_num_a2GVF 3
#   define RANK_num_a2SFSBC 3
#   define RANK_num_a2SFSBS 3
#   define RANK_num_a2SFSIC 3
#   define RANK_num_a2SFSIS 3
#   define RANK_num_a2SFWET 3
#   define RANK_num_a2TBF 3
#   define RANK_num_a2_CLXF 3
#   define RANK_num_a2_XFRC 4
#   define RANK_num_a2_sfcoag1 3
#   define RANK_num_a2_sfcsiz1 3
#   define RANK_num_a2_sfcsiz2 3
#   define RANK_num_a2_sfcsiz3 3
#   define RANK_num_a2_sfcsiz4 3
#   define RANK_num_a2_sfgaex2 3
#   define RANK_num_a2_sfnnuc1 3
#   define RANK_num_a3 4
#   define RANK_num_a3DDF 3
#   define RANK_num_a3GVF 3
#   define RANK_num_a3SFSBC 3
#   define RANK_num_a3SFSBS 3
#   define RANK_num_a3SFSIC 3
#   define RANK_num_a3SFSIS 3
#   define RANK_num_a3SFWET 3
#   define RANK_num_a3TBF 3
#   define RANK_num_a3_sfcsiz1 3
#   define RANK_num_a3_sfcsiz2 3
#   define RANK_num_c1 4
#   define RANK_num_c1DDF 3
#   define RANK_num_c1GVF 3
#   define RANK_num_c1SFSBC 3
#   define RANK_num_c1SFSBS 3
#   define RANK_num_c1SFSIC 3
#   define RANK_num_c1SFSIS 3
#   define RANK_num_c1SFWET 3
#   define RANK_num_c1TBF 3
#   define RANK_num_c1_sfcsiz1 3
#   define RANK_num_c1_sfcsiz2 3
#   define RANK_num_c1_sfcsiz3 3
#   define RANK_num_c1_sfcsiz4 3
#   define RANK_num_c1_sfgaex2 3
#   define RANK_num_c2 4
#   define RANK_num_c2DDF 3
#   define RANK_num_c2GVF 3
#   define RANK_num_c2SFSBC 3
#   define RANK_num_c2SFSBS 3
#   define RANK_num_c2SFSIC 3
#   define RANK_num_c2SFSIS 3
#   define RANK_num_c2SFWET 3
#   define RANK_num_c2TBF 3
#   define RANK_num_c2_sfcsiz1 3
#   define RANK_num_c2_sfcsiz2 3
#   define RANK_num_c2_sfcsiz3 3
#   define RANK_num_c2_sfcsiz4 3
#   define RANK_num_c2_sfgaex2 3
#   define RANK_num_c3 4
#   define RANK_num_c3DDF 3
#   define RANK_num_c3GVF 3
#   define RANK_num_c3SFSBC 3
#   define RANK_num_c3SFSBS 3
#   define RANK_num_c3SFSIC 3
#   define RANK_num_c3SFSIS 3
#   define RANK_num_c3SFWET 3
#   define RANK_num_c3TBF 3
#   define RANK_num_c3_sfcsiz1 3
#   define RANK_num_c3_sfcsiz2 3
#   define RANK_pom_a1 4
#   define RANK_pom_a1DDF 3
#   define RANK_pom_a1GVF 3
#   define RANK_pom_a1SFSBC 3
#   define RANK_pom_a1SFSBS 3
#   define RANK_pom_a1SFSIC 3
#   define RANK_pom_a1SFSIS 3
#   define RANK_pom_a1SFWET 3
#   define RANK_pom_a1TBF 3
#   define RANK_pom_a1_CLXF 3
#   define RANK_pom_a1_XFRC 4
#   define RANK_pom_c1 4
#   define RANK_pom_c1DDF 3
#   define RANK_pom_c1GVF 3
#   define RANK_pom_c1SFSBC 3
#   define RANK_pom_c1SFSBS 3
#   define RANK_pom_c1SFSIC 3
#   define RANK_pom_c1SFSIS 3
#   define RANK_pom_c1SFWET 3
#   define RANK_pom_c1TBF 3
#   define RANK_so4_a1 4
#   define RANK_so4_a1DDF 3
#   define RANK_so4_a1GVF 3
#   define RANK_so4_a1SFSBC 3
#   define RANK_so4_a1SFSBS 3
#   define RANK_so4_a1SFSIC 3
#   define RANK_so4_a1SFSIS 3
#   define RANK_so4_a1SFWET 3
#   define RANK_so4_a1TBF 3
#   define RANK_so4_a1_CLXF 3
#   define RANK_so4_a1_XFRC 4
#   define RANK_so4_a1_sfcoag1 3
#   define RANK_so4_a1_sfcsiz3 3
#   define RANK_so4_a1_sfcsiz4 3
#   define RANK_so4_a1_sfgaex1 3
#   define RANK_so4_a1_sfgaex2 3
#   define RANK_so4_a2 4
#   define RANK_so4_a2DDF 3
#   define RANK_so4_a2GVF 3
#   define RANK_so4_a2SFSBC 3
#   define RANK_so4_a2SFSBS 3
#   define RANK_so4_a2SFSIC 3
#   define RANK_so4_a2SFSIS 3
#   define RANK_so4_a2SFWET 3
#   define RANK_so4_a2TBF 3
#   define RANK_so4_a2_CLXF 3
#   define RANK_so4_a2_XFRC 4
#   define RANK_so4_a2_sfcoag1 3
#   define RANK_so4_a2_sfcsiz3 3
#   define RANK_so4_a2_sfcsiz4 3
#   define RANK_so4_a2_sfgaex1 3
#   define RANK_so4_a2_sfgaex2 3
#   define RANK_so4_a2_sfnnuc1 3
#   define RANK_so4_a3 4
#   define RANK_so4_a3DDF 3
#   define RANK_so4_a3GVF 3
#   define RANK_so4_a3SFSBC 3
#   define RANK_so4_a3SFSBS 3
#   define RANK_so4_a3SFSIC 3
#   define RANK_so4_a3SFSIS 3
#   define RANK_so4_a3SFWET 3
#   define RANK_so4_a3TBF 3
#   define RANK_so4_a3_sfgaex1 3
#   define RANK_so4_c1 4
#   define RANK_so4_c1AQH2SO4 3
#   define RANK_so4_c1AQSO4 3
#   define RANK_so4_c1DDF 3
#   define RANK_so4_c1GVF 3
#   define RANK_so4_c1SFSBC 3
#   define RANK_so4_c1SFSBS 3
#   define RANK_so4_c1SFSIC 3
#   define RANK_so4_c1SFSIS 3
#   define RANK_so4_c1SFWET 3
#   define RANK_so4_c1TBF 3
#   define RANK_so4_c1_sfcsiz3 3
#   define RANK_so4_c1_sfcsiz4 3
#   define RANK_so4_c1_sfgaex2 3
#   define RANK_so4_c2 4
#   define RANK_so4_c2AQH2SO4 3
#   define RANK_so4_c2AQSO4 3
#   define RANK_so4_c2DDF 3
#   define RANK_so4_c2GVF 3
#   define RANK_so4_c2SFSBC 3
#   define RANK_so4_c2SFSBS 3
#   define RANK_so4_c2SFSIC 3
#   define RANK_so4_c2SFSIS 3
#   define RANK_so4_c2SFWET 3
#   define RANK_so4_c2TBF 3
#   define RANK_so4_c2_sfcsiz3 3
#   define RANK_so4_c2_sfcsiz4 3
#   define RANK_so4_c2_sfgaex2 3
#   define RANK_so4_c3 4
#   define RANK_so4_c3AQH2SO4 3
#   define RANK_so4_c3AQSO4 3
#   define RANK_so4_c3DDF 3
#   define RANK_so4_c3GVF 3
#   define RANK_so4_c3SFSBC 3
#   define RANK_so4_c3SFSBS 3
#   define RANK_so4_c3SFSIC 3
#   define RANK_so4_c3SFSIS 3
#   define RANK_so4_c3SFWET 3
#   define RANK_so4_c3TBF 3
#   define RANK_soa_a1 4
#   define RANK_soa_a1DDF 3
#   define RANK_soa_a1GVF 3
#   define RANK_soa_a1SFSBC 3
#   define RANK_soa_a1SFSBS 3
#   define RANK_soa_a1SFSIC 3
#   define RANK_soa_a1SFSIS 3
#   define RANK_soa_a1SFWET 3
#   define RANK_soa_a1TBF 3
#   define RANK_soa_a1_sfcoag1 3
#   define RANK_soa_a1_sfcsiz3 3
#   define RANK_soa_a1_sfcsiz4 3
#   define RANK_soa_a1_sfgaex1 3
#   define RANK_soa_a1_sfgaex2 3
#   define RANK_soa_a2 4
#   define RANK_soa_a2DDF 3
#   define RANK_soa_a2GVF 3
#   define RANK_soa_a2SFSBC 3
#   define RANK_soa_a2SFSBS 3
#   define RANK_soa_a2SFSIC 3
#   define RANK_soa_a2SFSIS 3
#   define RANK_soa_a2SFWET 3
#   define RANK_soa_a2TBF 3
#   define RANK_soa_a2_sfcoag1 3
#   define RANK_soa_a2_sfcsiz3 3
#   define RANK_soa_a2_sfcsiz4 3
#   define RANK_soa_a2_sfgaex1 3
#   define RANK_soa_a2_sfgaex2 3
#   define RANK_soa_c1 4
#   define RANK_soa_c1DDF 3
#   define RANK_soa_c1GVF 3
#   define RANK_soa_c1SFSBC 3
#   define RANK_soa_c1SFSBS 3
#   define RANK_soa_c1SFSIC 3
#   define RANK_soa_c1SFSIS 3
#   define RANK_soa_c1SFWET 3
#   define RANK_soa_c1TBF 3
#   define RANK_soa_c1_sfcsiz3 3
#   define RANK_soa_c1_sfcsiz4 3
#   define RANK_soa_c1_sfgaex2 3
#   define RANK_soa_c2 4
#   define RANK_soa_c2DDF 3
#   define RANK_soa_c2GVF 3
#   define RANK_soa_c2SFSBC 3
#   define RANK_soa_c2SFSBS 3
#   define RANK_soa_c2SFSIC 3
#   define RANK_soa_c2SFSIS 3
#   define RANK_soa_c2SFWET 3
#   define RANK_soa_c2TBF 3
#   define RANK_soa_c2_sfcsiz3 3
#   define RANK_soa_c2_sfcsiz4 3
#   define RANK_soa_c2_sfgaex2 3
#   define RANK_wat_a1 4
#   define RANK_wat_a2 4
#   define RANK_wat_a3 4

    /* variable shapes */
    int lat_dims[RANK_lat];
    int lon_dims[RANK_lon];
    int slat_dims[RANK_slat];
    int slon_dims[RANK_slon];
    int w_stag_dims[RANK_w_stag];
    int lev_dims[RANK_lev];
    int ilev_dims[RANK_ilev];
    int isccp_prs_dims[RANK_isccp_prs];
    int isccp_tau_dims[RANK_isccp_tau];
    int isccp_prstau_dims[RANK_isccp_prstau];
    int time_dims[RANK_time];
    int time_bnds_dims[RANK_time_bnds];
    int date_written_dims[RANK_date_written];
    int time_written_dims[RANK_time_written];
    int nlon_dims[RANK_nlon];
    int wnummax_dims[RANK_wnummax];
    int hyai_dims[RANK_hyai];
    int hybi_dims[RANK_hybi];
    int hyam_dims[RANK_hyam];
    int hybm_dims[RANK_hybm];
    int gw_dims[RANK_gw];
    int ndcur_dims[RANK_ndcur];
    int nscur_dims[RANK_nscur];
    int date_dims[RANK_date];
    int co2vmr_dims[RANK_co2vmr];
    int ch4vmr_dims[RANK_ch4vmr];
    int n2ovmr_dims[RANK_n2ovmr];
    int f11vmr_dims[RANK_f11vmr];
    int f12vmr_dims[RANK_f12vmr];
    int sol_tsi_dims[RANK_sol_tsi];
    int datesec_dims[RANK_datesec];
    int nsteph_dims[RANK_nsteph];
    int ABSORB_dims[RANK_ABSORB];
    int AEROD_v_dims[RANK_AEROD_v];
    int AODABS_dims[RANK_AODABS];
    int AODDUST1_dims[RANK_AODDUST1];
    int AODDUST2_dims[RANK_AODDUST2];
    int AODDUST3_dims[RANK_AODDUST3];
    int AODMODE1_dims[RANK_AODMODE1];
    int AODMODE2_dims[RANK_AODMODE2];
    int AODMODE3_dims[RANK_AODMODE3];
    int AODVIS_dims[RANK_AODVIS];
    int AQSO4_H2O2_dims[RANK_AQSO4_H2O2];
    int AQSO4_O3_dims[RANK_AQSO4_O3];
    int AQ_DMS_dims[RANK_AQ_DMS];
    int AQ_H2O2_dims[RANK_AQ_H2O2];
    int AQ_H2SO4_dims[RANK_AQ_H2SO4];
    int AQ_SO2_dims[RANK_AQ_SO2];
    int AQ_SOAG_dims[RANK_AQ_SOAG];
    int AQ_bc_a1_dims[RANK_AQ_bc_a1];
    int AQ_dst_a1_dims[RANK_AQ_dst_a1];
    int AQ_dst_a3_dims[RANK_AQ_dst_a3];
    int AQ_ncl_a1_dims[RANK_AQ_ncl_a1];
    int AQ_ncl_a2_dims[RANK_AQ_ncl_a2];
    int AQ_ncl_a3_dims[RANK_AQ_ncl_a3];
    int AQ_num_a1_dims[RANK_AQ_num_a1];
    int AQ_num_a2_dims[RANK_AQ_num_a2];
    int AQ_num_a3_dims[RANK_AQ_num_a3];
    int AQ_pom_a1_dims[RANK_AQ_pom_a1];
    int AQ_so4_a1_dims[RANK_AQ_so4_a1];
    int AQ_so4_a2_dims[RANK_AQ_so4_a2];
    int AQ_so4_a3_dims[RANK_AQ_so4_a3];
    int AQ_soa_a1_dims[RANK_AQ_soa_a1];
    int AQ_soa_a2_dims[RANK_AQ_soa_a2];
    int BPROD_dims[RANK_BPROD];
    int BURDEN1_dims[RANK_BURDEN1];
    int BURDEN2_dims[RANK_BURDEN2];
    int BURDEN3_dims[RANK_BURDEN3];
    int CCN1_dims[RANK_CCN1];
    int CCN2_dims[RANK_CCN2];
    int CCN3_dims[RANK_CCN3];
    int CCN4_dims[RANK_CCN4];
    int CCN5_dims[RANK_CCN5];
    int CCN6_dims[RANK_CCN6];
    int CLDHGH_dims[RANK_CLDHGH];
    int CLDICE_dims[RANK_CLDICE];
    int CLDLIQ_dims[RANK_CLDLIQ];
    int CLDLOW_dims[RANK_CLDLOW];
    int CLDMED_dims[RANK_CLDMED];
    int CLDTOT_dims[RANK_CLDTOT];
    int CLOUD_dims[RANK_CLOUD];
    int CMFDQ_dims[RANK_CMFDQ];
    int CMFDQR_dims[RANK_CMFDQR];
    int CMFDT_dims[RANK_CMFDT];
    int CMFMC_dims[RANK_CMFMC];
    int CMFMCDZM_dims[RANK_CMFMCDZM];
    int CONCLD_dims[RANK_CONCLD];
    int DCQ_dims[RANK_DCQ];
    int DMS_dims[RANK_DMS];
    int DSTODXC_dims[RANK_DSTODXC];
    int DSTSFDRY_dims[RANK_DSTSFDRY];
    int DSTSFMBL_dims[RANK_DSTSFMBL];
    int DSTSFWET_dims[RANK_DSTSFWET];
    int DTCOND_dims[RANK_DTCOND];
    int DTV_dims[RANK_DTV];
    int EXTINCT_dims[RANK_EXTINCT];
    int FICE_dims[RANK_FICE];
    int FLDS_dims[RANK_FLDS];
    int FLNS_dims[RANK_FLNS];
    int FLNSC_dims[RANK_FLNSC];
    int FLNT_dims[RANK_FLNT];
    int FLNTC_dims[RANK_FLNTC];
    int FLUT_dims[RANK_FLUT];
    int FLUTC_dims[RANK_FLUTC];
    int FREQSH_dims[RANK_FREQSH];
    int FREQZM_dims[RANK_FREQZM];
    int FSDS_dims[RANK_FSDS];
    int FSDSC_dims[RANK_FSDSC];
    int FSNS_dims[RANK_FSNS];
    int FSNSC_dims[RANK_FSNSC];
    int FSNT_dims[RANK_FSNT];
    int FSNTC_dims[RANK_FSNTC];
    int FSNTOA_dims[RANK_FSNTOA];
    int FSNTOAC_dims[RANK_FSNTOAC];
    int FSUTOA_dims[RANK_FSUTOA];
    int GS_DMS_dims[RANK_GS_DMS];
    int GS_H2O2_dims[RANK_GS_H2O2];
    int GS_H2SO4_dims[RANK_GS_H2SO4];
    int GS_SO2_dims[RANK_GS_SO2];
    int GS_SOAG_dims[RANK_GS_SOAG];
    int GS_bc_a1_dims[RANK_GS_bc_a1];
    int GS_dst_a1_dims[RANK_GS_dst_a1];
    int GS_dst_a3_dims[RANK_GS_dst_a3];
    int GS_ncl_a1_dims[RANK_GS_ncl_a1];
    int GS_ncl_a2_dims[RANK_GS_ncl_a2];
    int GS_ncl_a3_dims[RANK_GS_ncl_a3];
    int GS_num_a1_dims[RANK_GS_num_a1];
    int GS_num_a2_dims[RANK_GS_num_a2];
    int GS_num_a3_dims[RANK_GS_num_a3];
    int GS_pom_a1_dims[RANK_GS_pom_a1];
    int GS_so4_a1_dims[RANK_GS_so4_a1];
    int GS_so4_a2_dims[RANK_GS_so4_a2];
    int GS_so4_a3_dims[RANK_GS_so4_a3];
    int GS_soa_a1_dims[RANK_GS_soa_a1];
    int GS_soa_a2_dims[RANK_GS_soa_a2];
    int H2O2_dims[RANK_H2O2];
    int H2SO4_dims[RANK_H2SO4];
    int H2SO4_sfgaex1_dims[RANK_H2SO4_sfgaex1];
    int H2SO4_sfnnuc1_dims[RANK_H2SO4_sfnnuc1];
    int ICEFRAC_dims[RANK_ICEFRAC];
    int ICIMR_dims[RANK_ICIMR];
    int ICWMR_dims[RANK_ICWMR];
    int KVH_dims[RANK_KVH];
    int KVM_dims[RANK_KVM];
    int LANDFRAC_dims[RANK_LANDFRAC];
    int LCLOUD_dims[RANK_LCLOUD];
    int LHFLX_dims[RANK_LHFLX];
    int LND_MBL_dims[RANK_LND_MBL];
    int LWCF_dims[RANK_LWCF];
    int NDROPCOL_dims[RANK_NDROPCOL];
    int NDROPMIX_dims[RANK_NDROPMIX];
    int NDROPSNK_dims[RANK_NDROPSNK];
    int NDROPSRC_dims[RANK_NDROPSRC];
    int NUMICE_dims[RANK_NUMICE];
    int NUMLIQ_dims[RANK_NUMLIQ];
    int OCNFRAC_dims[RANK_OCNFRAC];
    int ODV_bc_a1_dims[RANK_ODV_bc_a1];
    int ODV_dst_a1_dims[RANK_ODV_dst_a1];
    int ODV_dst_a3_dims[RANK_ODV_dst_a3];
    int ODV_ncl_a1_dims[RANK_ODV_ncl_a1];
    int ODV_ncl_a3_dims[RANK_ODV_ncl_a3];
    int ODV_pom_a1_dims[RANK_ODV_pom_a1];
    int ODV_so4_a1_dims[RANK_ODV_so4_a1];
    int ODV_soa_a1_dims[RANK_ODV_soa_a1];
    int OMEGA_dims[RANK_OMEGA];
    int OMEGAT_dims[RANK_OMEGAT];
    int ORO_dims[RANK_ORO];
    int PBLH_dims[RANK_PBLH];
    int PCONVB_dims[RANK_PCONVB];
    int PCONVT_dims[RANK_PCONVT];
    int PHIS_dims[RANK_PHIS];
    int PRECC_dims[RANK_PRECC];
    int PRECCDZM_dims[RANK_PRECCDZM];
    int PRECL_dims[RANK_PRECL];
    int PRECSC_dims[RANK_PRECSC];
    int PRECSH_dims[RANK_PRECSH];
    int PRECSL_dims[RANK_PRECSL];
    int PRECT_dims[RANK_PRECT];
    int PS_dims[RANK_PS];
    int PSL_dims[RANK_PSL];
    int Q_dims[RANK_Q];
    int QC_dims[RANK_QC];
    int QFLX_dims[RANK_QFLX];
    int QREFHT_dims[RANK_QREFHT];
    int QRL_dims[RANK_QRL];
    int QRS_dims[RANK_QRS];
    int QT_dims[RANK_QT];
    int QTFLX_dims[RANK_QTFLX];
    int RAM1_dims[RANK_RAM1];
    int RELHUM_dims[RANK_RELHUM];
    int RHREFHT_dims[RANK_RHREFHT];
    int SFCLDICE_dims[RANK_SFCLDICE];
    int SFCLDLIQ_dims[RANK_SFCLDLIQ];
    int SFI_dims[RANK_SFI];
    int SFNUMICE_dims[RANK_SFNUMICE];
    int SFNUMLIQ_dims[RANK_SFNUMLIQ];
    int SHFLX_dims[RANK_SHFLX];
    int SL_dims[RANK_SL];
    int SLFLX_dims[RANK_SLFLX];
    int SLV_dims[RANK_SLV];
    int SNOWHICE_dims[RANK_SNOWHICE];
    int SNOWHLND_dims[RANK_SNOWHLND];
    int SO2_dims[RANK_SO2];
    int SO2_CLXF_dims[RANK_SO2_CLXF];
    int SO2_XFRC_dims[RANK_SO2_XFRC];
    int SOAG_dims[RANK_SOAG];
    int SOAG_sfgaex1_dims[RANK_SOAG_sfgaex1];
    int SOLIN_dims[RANK_SOLIN];
    int SPROD_dims[RANK_SPROD];
    int SRFRAD_dims[RANK_SRFRAD];
    int SSAVIS_dims[RANK_SSAVIS];
    int SSTODXC_dims[RANK_SSTODXC];
    int SSTSFDRY_dims[RANK_SSTSFDRY];
    int SSTSFMBL_dims[RANK_SSTSFMBL];
    int SSTSFWET_dims[RANK_SSTSFWET];
    int SWCF_dims[RANK_SWCF];
    int T_dims[RANK_T];
    int TAUTMSX_dims[RANK_TAUTMSX];
    int TAUTMSY_dims[RANK_TAUTMSY];
    int TAUX_dims[RANK_TAUX];
    int TAUY_dims[RANK_TAUY];
    int TGCLDCWP_dims[RANK_TGCLDCWP];
    int TGCLDIWP_dims[RANK_TGCLDIWP];
    int TGCLDLWP_dims[RANK_TGCLDLWP];
    int TKE_dims[RANK_TKE];
    int TMQ_dims[RANK_TMQ];
    int TREFHT_dims[RANK_TREFHT];
    int TREFMNAV_dims[RANK_TREFMNAV];
    int TREFMXAV_dims[RANK_TREFMXAV];
    int TROP_FD_dims[RANK_TROP_FD];
    int TROP_P_dims[RANK_TROP_P];
    int TROP_PD_dims[RANK_TROP_PD];
    int TROP_T_dims[RANK_TROP_T];
    int TROP_Z_dims[RANK_TROP_Z];
    int TS_dims[RANK_TS];
    int TSMN_dims[RANK_TSMN];
    int TSMX_dims[RANK_TSMX];
    int U_dims[RANK_U];
    int UFLX_dims[RANK_UFLX];
    int US_dims[RANK_US];
    int UU_dims[RANK_UU];
    int V_dims[RANK_V];
    int VD01_dims[RANK_VD01];
    int VFLX_dims[RANK_VFLX];
    int VQ_dims[RANK_VQ];
    int VS_dims[RANK_VS];
    int VT_dims[RANK_VT];
    int VU_dims[RANK_VU];
    int VV_dims[RANK_VV];
    int WGUSTD_dims[RANK_WGUSTD];
    int WTKE_dims[RANK_WTKE];
    int XPH_LWC_dims[RANK_XPH_LWC];
    int Z3_dims[RANK_Z3];
    int airFV_dims[RANK_airFV];
    int bc_a1_dims[RANK_bc_a1];
    int bc_a1DDF_dims[RANK_bc_a1DDF];
    int bc_a1GVF_dims[RANK_bc_a1GVF];
    int bc_a1SFSBC_dims[RANK_bc_a1SFSBC];
    int bc_a1SFSBS_dims[RANK_bc_a1SFSBS];
    int bc_a1SFSIC_dims[RANK_bc_a1SFSIC];
    int bc_a1SFSIS_dims[RANK_bc_a1SFSIS];
    int bc_a1SFWET_dims[RANK_bc_a1SFWET];
    int bc_a1TBF_dims[RANK_bc_a1TBF];
    int bc_a1_CLXF_dims[RANK_bc_a1_CLXF];
    int bc_a1_XFRC_dims[RANK_bc_a1_XFRC];
    int bc_c1_dims[RANK_bc_c1];
    int bc_c1DDF_dims[RANK_bc_c1DDF];
    int bc_c1GVF_dims[RANK_bc_c1GVF];
    int bc_c1SFSBC_dims[RANK_bc_c1SFSBC];
    int bc_c1SFSBS_dims[RANK_bc_c1SFSBS];
    int bc_c1SFSIC_dims[RANK_bc_c1SFSIC];
    int bc_c1SFSIS_dims[RANK_bc_c1SFSIS];
    int bc_c1SFWET_dims[RANK_bc_c1SFWET];
    int bc_c1TBF_dims[RANK_bc_c1TBF];
    int chem_trop_dims[RANK_chem_trop];
    int chem_trop_tropop_dims[RANK_chem_trop_tropop];
    int dgnd_a01_dims[RANK_dgnd_a01];
    int dgnd_a02_dims[RANK_dgnd_a02];
    int dgnd_a03_dims[RANK_dgnd_a03];
    int dgnw_a01_dims[RANK_dgnw_a01];
    int dgnw_a02_dims[RANK_dgnw_a02];
    int dgnw_a03_dims[RANK_dgnw_a03];
    int dst_a1_dims[RANK_dst_a1];
    int dst_a1DDF_dims[RANK_dst_a1DDF];
    int dst_a1GVF_dims[RANK_dst_a1GVF];
    int dst_a1SF_dims[RANK_dst_a1SF];
    int dst_a1SFSBC_dims[RANK_dst_a1SFSBC];
    int dst_a1SFSBS_dims[RANK_dst_a1SFSBS];
    int dst_a1SFSIC_dims[RANK_dst_a1SFSIC];
    int dst_a1SFSIS_dims[RANK_dst_a1SFSIS];
    int dst_a1SFWET_dims[RANK_dst_a1SFWET];
    int dst_a1TBF_dims[RANK_dst_a1TBF];
    int dst_a3_dims[RANK_dst_a3];
    int dst_a3DDF_dims[RANK_dst_a3DDF];
    int dst_a3GVF_dims[RANK_dst_a3GVF];
    int dst_a3SF_dims[RANK_dst_a3SF];
    int dst_a3SFSBC_dims[RANK_dst_a3SFSBC];
    int dst_a3SFSBS_dims[RANK_dst_a3SFSBS];
    int dst_a3SFSIC_dims[RANK_dst_a3SFSIC];
    int dst_a3SFSIS_dims[RANK_dst_a3SFSIS];
    int dst_a3SFWET_dims[RANK_dst_a3SFWET];
    int dst_a3TBF_dims[RANK_dst_a3TBF];
    int dst_c1_dims[RANK_dst_c1];
    int dst_c1DDF_dims[RANK_dst_c1DDF];
    int dst_c1GVF_dims[RANK_dst_c1GVF];
    int dst_c1SFSBC_dims[RANK_dst_c1SFSBC];
    int dst_c1SFSBS_dims[RANK_dst_c1SFSBS];
    int dst_c1SFSIC_dims[RANK_dst_c1SFSIC];
    int dst_c1SFSIS_dims[RANK_dst_c1SFSIS];
    int dst_c1SFWET_dims[RANK_dst_c1SFWET];
    int dst_c1TBF_dims[RANK_dst_c1TBF];
    int dst_c3_dims[RANK_dst_c3];
    int dst_c3DDF_dims[RANK_dst_c3DDF];
    int dst_c3GVF_dims[RANK_dst_c3GVF];
    int dst_c3SFSBC_dims[RANK_dst_c3SFSBC];
    int dst_c3SFSBS_dims[RANK_dst_c3SFSBS];
    int dst_c3SFSIC_dims[RANK_dst_c3SFSIC];
    int dst_c3SFSIS_dims[RANK_dst_c3SFSIS];
    int dst_c3SFWET_dims[RANK_dst_c3SFWET];
    int dst_c3TBF_dims[RANK_dst_c3TBF];
    int ncl_a1_dims[RANK_ncl_a1];
    int ncl_a1DDF_dims[RANK_ncl_a1DDF];
    int ncl_a1GVF_dims[RANK_ncl_a1GVF];
    int ncl_a1SF_dims[RANK_ncl_a1SF];
    int ncl_a1SFSBC_dims[RANK_ncl_a1SFSBC];
    int ncl_a1SFSBS_dims[RANK_ncl_a1SFSBS];
    int ncl_a1SFSIC_dims[RANK_ncl_a1SFSIC];
    int ncl_a1SFSIS_dims[RANK_ncl_a1SFSIS];
    int ncl_a1SFWET_dims[RANK_ncl_a1SFWET];
    int ncl_a1TBF_dims[RANK_ncl_a1TBF];
    int ncl_a1_sfcoag1_dims[RANK_ncl_a1_sfcoag1];
    int ncl_a1_sfcsiz3_dims[RANK_ncl_a1_sfcsiz3];
    int ncl_a1_sfcsiz4_dims[RANK_ncl_a1_sfcsiz4];
    int ncl_a1_sfgaex2_dims[RANK_ncl_a1_sfgaex2];
    int ncl_a2_dims[RANK_ncl_a2];
    int ncl_a2DDF_dims[RANK_ncl_a2DDF];
    int ncl_a2GVF_dims[RANK_ncl_a2GVF];
    int ncl_a2SF_dims[RANK_ncl_a2SF];
    int ncl_a2SFSBC_dims[RANK_ncl_a2SFSBC];
    int ncl_a2SFSBS_dims[RANK_ncl_a2SFSBS];
    int ncl_a2SFSIC_dims[RANK_ncl_a2SFSIC];
    int ncl_a2SFSIS_dims[RANK_ncl_a2SFSIS];
    int ncl_a2SFWET_dims[RANK_ncl_a2SFWET];
    int ncl_a2TBF_dims[RANK_ncl_a2TBF];
    int ncl_a2_sfcoag1_dims[RANK_ncl_a2_sfcoag1];
    int ncl_a2_sfcsiz3_dims[RANK_ncl_a2_sfcsiz3];
    int ncl_a2_sfcsiz4_dims[RANK_ncl_a2_sfcsiz4];
    int ncl_a2_sfgaex2_dims[RANK_ncl_a2_sfgaex2];
    int ncl_a3_dims[RANK_ncl_a3];
    int ncl_a3DDF_dims[RANK_ncl_a3DDF];
    int ncl_a3GVF_dims[RANK_ncl_a3GVF];
    int ncl_a3SF_dims[RANK_ncl_a3SF];
    int ncl_a3SFSBC_dims[RANK_ncl_a3SFSBC];
    int ncl_a3SFSBS_dims[RANK_ncl_a3SFSBS];
    int ncl_a3SFSIC_dims[RANK_ncl_a3SFSIC];
    int ncl_a3SFSIS_dims[RANK_ncl_a3SFSIS];
    int ncl_a3SFWET_dims[RANK_ncl_a3SFWET];
    int ncl_a3TBF_dims[RANK_ncl_a3TBF];
    int ncl_c1_dims[RANK_ncl_c1];
    int ncl_c1DDF_dims[RANK_ncl_c1DDF];
    int ncl_c1GVF_dims[RANK_ncl_c1GVF];
    int ncl_c1SFSBC_dims[RANK_ncl_c1SFSBC];
    int ncl_c1SFSBS_dims[RANK_ncl_c1SFSBS];
    int ncl_c1SFSIC_dims[RANK_ncl_c1SFSIC];
    int ncl_c1SFSIS_dims[RANK_ncl_c1SFSIS];
    int ncl_c1SFWET_dims[RANK_ncl_c1SFWET];
    int ncl_c1TBF_dims[RANK_ncl_c1TBF];
    int ncl_c1_sfcsiz3_dims[RANK_ncl_c1_sfcsiz3];
    int ncl_c1_sfcsiz4_dims[RANK_ncl_c1_sfcsiz4];
    int ncl_c1_sfgaex2_dims[RANK_ncl_c1_sfgaex2];
    int ncl_c2_dims[RANK_ncl_c2];
    int ncl_c2DDF_dims[RANK_ncl_c2DDF];
    int ncl_c2GVF_dims[RANK_ncl_c2GVF];
    int ncl_c2SFSBC_dims[RANK_ncl_c2SFSBC];
    int ncl_c2SFSBS_dims[RANK_ncl_c2SFSBS];
    int ncl_c2SFSIC_dims[RANK_ncl_c2SFSIC];
    int ncl_c2SFSIS_dims[RANK_ncl_c2SFSIS];
    int ncl_c2SFWET_dims[RANK_ncl_c2SFWET];
    int ncl_c2TBF_dims[RANK_ncl_c2TBF];
    int ncl_c2_sfcsiz3_dims[RANK_ncl_c2_sfcsiz3];
    int ncl_c2_sfcsiz4_dims[RANK_ncl_c2_sfcsiz4];
    int ncl_c2_sfgaex2_dims[RANK_ncl_c2_sfgaex2];
    int ncl_c3_dims[RANK_ncl_c3];
    int ncl_c3DDF_dims[RANK_ncl_c3DDF];
    int ncl_c3GVF_dims[RANK_ncl_c3GVF];
    int ncl_c3SFSBC_dims[RANK_ncl_c3SFSBC];
    int ncl_c3SFSBS_dims[RANK_ncl_c3SFSBS];
    int ncl_c3SFSIC_dims[RANK_ncl_c3SFSIC];
    int ncl_c3SFSIS_dims[RANK_ncl_c3SFSIS];
    int ncl_c3SFWET_dims[RANK_ncl_c3SFWET];
    int ncl_c3TBF_dims[RANK_ncl_c3TBF];
    int num_a1_dims[RANK_num_a1];
    int num_a1DDF_dims[RANK_num_a1DDF];
    int num_a1GVF_dims[RANK_num_a1GVF];
    int num_a1SFSBC_dims[RANK_num_a1SFSBC];
    int num_a1SFSBS_dims[RANK_num_a1SFSBS];
    int num_a1SFSIC_dims[RANK_num_a1SFSIC];
    int num_a1SFSIS_dims[RANK_num_a1SFSIS];
    int num_a1SFWET_dims[RANK_num_a1SFWET];
    int num_a1TBF_dims[RANK_num_a1TBF];
    int num_a1_CLXF_dims[RANK_num_a1_CLXF];
    int num_a1_XFRC_dims[RANK_num_a1_XFRC];
    int num_a1_sfcoag1_dims[RANK_num_a1_sfcoag1];
    int num_a1_sfcsiz1_dims[RANK_num_a1_sfcsiz1];
    int num_a1_sfcsiz2_dims[RANK_num_a1_sfcsiz2];
    int num_a1_sfcsiz3_dims[RANK_num_a1_sfcsiz3];
    int num_a1_sfcsiz4_dims[RANK_num_a1_sfcsiz4];
    int num_a1_sfgaex2_dims[RANK_num_a1_sfgaex2];
    int num_a2_dims[RANK_num_a2];
    int num_a2DDF_dims[RANK_num_a2DDF];
    int num_a2GVF_dims[RANK_num_a2GVF];
    int num_a2SFSBC_dims[RANK_num_a2SFSBC];
    int num_a2SFSBS_dims[RANK_num_a2SFSBS];
    int num_a2SFSIC_dims[RANK_num_a2SFSIC];
    int num_a2SFSIS_dims[RANK_num_a2SFSIS];
    int num_a2SFWET_dims[RANK_num_a2SFWET];
    int num_a2TBF_dims[RANK_num_a2TBF];
    int num_a2_CLXF_dims[RANK_num_a2_CLXF];
    int num_a2_XFRC_dims[RANK_num_a2_XFRC];
    int num_a2_sfcoag1_dims[RANK_num_a2_sfcoag1];
    int num_a2_sfcsiz1_dims[RANK_num_a2_sfcsiz1];
    int num_a2_sfcsiz2_dims[RANK_num_a2_sfcsiz2];
    int num_a2_sfcsiz3_dims[RANK_num_a2_sfcsiz3];
    int num_a2_sfcsiz4_dims[RANK_num_a2_sfcsiz4];
    int num_a2_sfgaex2_dims[RANK_num_a2_sfgaex2];
    int num_a2_sfnnuc1_dims[RANK_num_a2_sfnnuc1];
    int num_a3_dims[RANK_num_a3];
    int num_a3DDF_dims[RANK_num_a3DDF];
    int num_a3GVF_dims[RANK_num_a3GVF];
    int num_a3SFSBC_dims[RANK_num_a3SFSBC];
    int num_a3SFSBS_dims[RANK_num_a3SFSBS];
    int num_a3SFSIC_dims[RANK_num_a3SFSIC];
    int num_a3SFSIS_dims[RANK_num_a3SFSIS];
    int num_a3SFWET_dims[RANK_num_a3SFWET];
    int num_a3TBF_dims[RANK_num_a3TBF];
    int num_a3_sfcsiz1_dims[RANK_num_a3_sfcsiz1];
    int num_a3_sfcsiz2_dims[RANK_num_a3_sfcsiz2];
    int num_c1_dims[RANK_num_c1];
    int num_c1DDF_dims[RANK_num_c1DDF];
    int num_c1GVF_dims[RANK_num_c1GVF];
    int num_c1SFSBC_dims[RANK_num_c1SFSBC];
    int num_c1SFSBS_dims[RANK_num_c1SFSBS];
    int num_c1SFSIC_dims[RANK_num_c1SFSIC];
    int num_c1SFSIS_dims[RANK_num_c1SFSIS];
    int num_c1SFWET_dims[RANK_num_c1SFWET];
    int num_c1TBF_dims[RANK_num_c1TBF];
    int num_c1_sfcsiz1_dims[RANK_num_c1_sfcsiz1];
    int num_c1_sfcsiz2_dims[RANK_num_c1_sfcsiz2];
    int num_c1_sfcsiz3_dims[RANK_num_c1_sfcsiz3];
    int num_c1_sfcsiz4_dims[RANK_num_c1_sfcsiz4];
    int num_c1_sfgaex2_dims[RANK_num_c1_sfgaex2];
    int num_c2_dims[RANK_num_c2];
    int num_c2DDF_dims[RANK_num_c2DDF];
    int num_c2GVF_dims[RANK_num_c2GVF];
    int num_c2SFSBC_dims[RANK_num_c2SFSBC];
    int num_c2SFSBS_dims[RANK_num_c2SFSBS];
    int num_c2SFSIC_dims[RANK_num_c2SFSIC];
    int num_c2SFSIS_dims[RANK_num_c2SFSIS];
    int num_c2SFWET_dims[RANK_num_c2SFWET];
    int num_c2TBF_dims[RANK_num_c2TBF];
    int num_c2_sfcsiz1_dims[RANK_num_c2_sfcsiz1];
    int num_c2_sfcsiz2_dims[RANK_num_c2_sfcsiz2];
    int num_c2_sfcsiz3_dims[RANK_num_c2_sfcsiz3];
    int num_c2_sfcsiz4_dims[RANK_num_c2_sfcsiz4];
    int num_c2_sfgaex2_dims[RANK_num_c2_sfgaex2];
    int num_c3_dims[RANK_num_c3];
    int num_c3DDF_dims[RANK_num_c3DDF];
    int num_c3GVF_dims[RANK_num_c3GVF];
    int num_c3SFSBC_dims[RANK_num_c3SFSBC];
    int num_c3SFSBS_dims[RANK_num_c3SFSBS];
    int num_c3SFSIC_dims[RANK_num_c3SFSIC];
    int num_c3SFSIS_dims[RANK_num_c3SFSIS];
    int num_c3SFWET_dims[RANK_num_c3SFWET];
    int num_c3TBF_dims[RANK_num_c3TBF];
    int num_c3_sfcsiz1_dims[RANK_num_c3_sfcsiz1];
    int num_c3_sfcsiz2_dims[RANK_num_c3_sfcsiz2];
    int pom_a1_dims[RANK_pom_a1];
    int pom_a1DDF_dims[RANK_pom_a1DDF];
    int pom_a1GVF_dims[RANK_pom_a1GVF];
    int pom_a1SFSBC_dims[RANK_pom_a1SFSBC];
    int pom_a1SFSBS_dims[RANK_pom_a1SFSBS];
    int pom_a1SFSIC_dims[RANK_pom_a1SFSIC];
    int pom_a1SFSIS_dims[RANK_pom_a1SFSIS];
    int pom_a1SFWET_dims[RANK_pom_a1SFWET];
    int pom_a1TBF_dims[RANK_pom_a1TBF];
    int pom_a1_CLXF_dims[RANK_pom_a1_CLXF];
    int pom_a1_XFRC_dims[RANK_pom_a1_XFRC];
    int pom_c1_dims[RANK_pom_c1];
    int pom_c1DDF_dims[RANK_pom_c1DDF];
    int pom_c1GVF_dims[RANK_pom_c1GVF];
    int pom_c1SFSBC_dims[RANK_pom_c1SFSBC];
    int pom_c1SFSBS_dims[RANK_pom_c1SFSBS];
    int pom_c1SFSIC_dims[RANK_pom_c1SFSIC];
    int pom_c1SFSIS_dims[RANK_pom_c1SFSIS];
    int pom_c1SFWET_dims[RANK_pom_c1SFWET];
    int pom_c1TBF_dims[RANK_pom_c1TBF];
    int so4_a1_dims[RANK_so4_a1];
    int so4_a1DDF_dims[RANK_so4_a1DDF];
    int so4_a1GVF_dims[RANK_so4_a1GVF];
    int so4_a1SFSBC_dims[RANK_so4_a1SFSBC];
    int so4_a1SFSBS_dims[RANK_so4_a1SFSBS];
    int so4_a1SFSIC_dims[RANK_so4_a1SFSIC];
    int so4_a1SFSIS_dims[RANK_so4_a1SFSIS];
    int so4_a1SFWET_dims[RANK_so4_a1SFWET];
    int so4_a1TBF_dims[RANK_so4_a1TBF];
    int so4_a1_CLXF_dims[RANK_so4_a1_CLXF];
    int so4_a1_XFRC_dims[RANK_so4_a1_XFRC];
    int so4_a1_sfcoag1_dims[RANK_so4_a1_sfcoag1];
    int so4_a1_sfcsiz3_dims[RANK_so4_a1_sfcsiz3];
    int so4_a1_sfcsiz4_dims[RANK_so4_a1_sfcsiz4];
    int so4_a1_sfgaex1_dims[RANK_so4_a1_sfgaex1];
    int so4_a1_sfgaex2_dims[RANK_so4_a1_sfgaex2];
    int so4_a2_dims[RANK_so4_a2];
    int so4_a2DDF_dims[RANK_so4_a2DDF];
    int so4_a2GVF_dims[RANK_so4_a2GVF];
    int so4_a2SFSBC_dims[RANK_so4_a2SFSBC];
    int so4_a2SFSBS_dims[RANK_so4_a2SFSBS];
    int so4_a2SFSIC_dims[RANK_so4_a2SFSIC];
    int so4_a2SFSIS_dims[RANK_so4_a2SFSIS];
    int so4_a2SFWET_dims[RANK_so4_a2SFWET];
    int so4_a2TBF_dims[RANK_so4_a2TBF];
    int so4_a2_CLXF_dims[RANK_so4_a2_CLXF];
    int so4_a2_XFRC_dims[RANK_so4_a2_XFRC];
    int so4_a2_sfcoag1_dims[RANK_so4_a2_sfcoag1];
    int so4_a2_sfcsiz3_dims[RANK_so4_a2_sfcsiz3];
    int so4_a2_sfcsiz4_dims[RANK_so4_a2_sfcsiz4];
    int so4_a2_sfgaex1_dims[RANK_so4_a2_sfgaex1];
    int so4_a2_sfgaex2_dims[RANK_so4_a2_sfgaex2];
    int so4_a2_sfnnuc1_dims[RANK_so4_a2_sfnnuc1];
    int so4_a3_dims[RANK_so4_a3];
    int so4_a3DDF_dims[RANK_so4_a3DDF];
    int so4_a3GVF_dims[RANK_so4_a3GVF];
    int so4_a3SFSBC_dims[RANK_so4_a3SFSBC];
    int so4_a3SFSBS_dims[RANK_so4_a3SFSBS];
    int so4_a3SFSIC_dims[RANK_so4_a3SFSIC];
    int so4_a3SFSIS_dims[RANK_so4_a3SFSIS];
    int so4_a3SFWET_dims[RANK_so4_a3SFWET];
    int so4_a3TBF_dims[RANK_so4_a3TBF];
    int so4_a3_sfgaex1_dims[RANK_so4_a3_sfgaex1];
    int so4_c1_dims[RANK_so4_c1];
    int so4_c1AQH2SO4_dims[RANK_so4_c1AQH2SO4];
    int so4_c1AQSO4_dims[RANK_so4_c1AQSO4];
    int so4_c1DDF_dims[RANK_so4_c1DDF];
    int so4_c1GVF_dims[RANK_so4_c1GVF];
    int so4_c1SFSBC_dims[RANK_so4_c1SFSBC];
    int so4_c1SFSBS_dims[RANK_so4_c1SFSBS];
    int so4_c1SFSIC_dims[RANK_so4_c1SFSIC];
    int so4_c1SFSIS_dims[RANK_so4_c1SFSIS];
    int so4_c1SFWET_dims[RANK_so4_c1SFWET];
    int so4_c1TBF_dims[RANK_so4_c1TBF];
    int so4_c1_sfcsiz3_dims[RANK_so4_c1_sfcsiz3];
    int so4_c1_sfcsiz4_dims[RANK_so4_c1_sfcsiz4];
    int so4_c1_sfgaex2_dims[RANK_so4_c1_sfgaex2];
    int so4_c2_dims[RANK_so4_c2];
    int so4_c2AQH2SO4_dims[RANK_so4_c2AQH2SO4];
    int so4_c2AQSO4_dims[RANK_so4_c2AQSO4];
    int so4_c2DDF_dims[RANK_so4_c2DDF];
    int so4_c2GVF_dims[RANK_so4_c2GVF];
    int so4_c2SFSBC_dims[RANK_so4_c2SFSBC];
    int so4_c2SFSBS_dims[RANK_so4_c2SFSBS];
    int so4_c2SFSIC_dims[RANK_so4_c2SFSIC];
    int so4_c2SFSIS_dims[RANK_so4_c2SFSIS];
    int so4_c2SFWET_dims[RANK_so4_c2SFWET];
    int so4_c2TBF_dims[RANK_so4_c2TBF];
    int so4_c2_sfcsiz3_dims[RANK_so4_c2_sfcsiz3];
    int so4_c2_sfcsiz4_dims[RANK_so4_c2_sfcsiz4];
    int so4_c2_sfgaex2_dims[RANK_so4_c2_sfgaex2];
    int so4_c3_dims[RANK_so4_c3];
    int so4_c3AQH2SO4_dims[RANK_so4_c3AQH2SO4];
    int so4_c3AQSO4_dims[RANK_so4_c3AQSO4];
    int so4_c3DDF_dims[RANK_so4_c3DDF];
    int so4_c3GVF_dims[RANK_so4_c3GVF];
    int so4_c3SFSBC_dims[RANK_so4_c3SFSBC];
    int so4_c3SFSBS_dims[RANK_so4_c3SFSBS];
    int so4_c3SFSIC_dims[RANK_so4_c3SFSIC];
    int so4_c3SFSIS_dims[RANK_so4_c3SFSIS];
    int so4_c3SFWET_dims[RANK_so4_c3SFWET];
    int so4_c3TBF_dims[RANK_so4_c3TBF];
    int soa_a1_dims[RANK_soa_a1];
    int soa_a1DDF_dims[RANK_soa_a1DDF];
    int soa_a1GVF_dims[RANK_soa_a1GVF];
    int soa_a1SFSBC_dims[RANK_soa_a1SFSBC];
    int soa_a1SFSBS_dims[RANK_soa_a1SFSBS];
    int soa_a1SFSIC_dims[RANK_soa_a1SFSIC];
    int soa_a1SFSIS_dims[RANK_soa_a1SFSIS];
    int soa_a1SFWET_dims[RANK_soa_a1SFWET];
    int soa_a1TBF_dims[RANK_soa_a1TBF];
    int soa_a1_sfcoag1_dims[RANK_soa_a1_sfcoag1];
    int soa_a1_sfcsiz3_dims[RANK_soa_a1_sfcsiz3];
    int soa_a1_sfcsiz4_dims[RANK_soa_a1_sfcsiz4];
    int soa_a1_sfgaex1_dims[RANK_soa_a1_sfgaex1];
    int soa_a1_sfgaex2_dims[RANK_soa_a1_sfgaex2];
    int soa_a2_dims[RANK_soa_a2];
    int soa_a2DDF_dims[RANK_soa_a2DDF];
    int soa_a2GVF_dims[RANK_soa_a2GVF];
    int soa_a2SFSBC_dims[RANK_soa_a2SFSBC];
    int soa_a2SFSBS_dims[RANK_soa_a2SFSBS];
    int soa_a2SFSIC_dims[RANK_soa_a2SFSIC];
    int soa_a2SFSIS_dims[RANK_soa_a2SFSIS];
    int soa_a2SFWET_dims[RANK_soa_a2SFWET];
    int soa_a2TBF_dims[RANK_soa_a2TBF];
    int soa_a2_sfcoag1_dims[RANK_soa_a2_sfcoag1];
    int soa_a2_sfcsiz3_dims[RANK_soa_a2_sfcsiz3];
    int soa_a2_sfcsiz4_dims[RANK_soa_a2_sfcsiz4];
    int soa_a2_sfgaex1_dims[RANK_soa_a2_sfgaex1];
    int soa_a2_sfgaex2_dims[RANK_soa_a2_sfgaex2];
    int soa_c1_dims[RANK_soa_c1];
    int soa_c1DDF_dims[RANK_soa_c1DDF];
    int soa_c1GVF_dims[RANK_soa_c1GVF];
    int soa_c1SFSBC_dims[RANK_soa_c1SFSBC];
    int soa_c1SFSBS_dims[RANK_soa_c1SFSBS];
    int soa_c1SFSIC_dims[RANK_soa_c1SFSIC];
    int soa_c1SFSIS_dims[RANK_soa_c1SFSIS];
    int soa_c1SFWET_dims[RANK_soa_c1SFWET];
    int soa_c1TBF_dims[RANK_soa_c1TBF];
    int soa_c1_sfcsiz3_dims[RANK_soa_c1_sfcsiz3];
    int soa_c1_sfcsiz4_dims[RANK_soa_c1_sfcsiz4];
    int soa_c1_sfgaex2_dims[RANK_soa_c1_sfgaex2];
    int soa_c2_dims[RANK_soa_c2];
    int soa_c2DDF_dims[RANK_soa_c2DDF];
    int soa_c2GVF_dims[RANK_soa_c2GVF];
    int soa_c2SFSBC_dims[RANK_soa_c2SFSBC];
    int soa_c2SFSBS_dims[RANK_soa_c2SFSBS];
    int soa_c2SFSIC_dims[RANK_soa_c2SFSIC];
    int soa_c2SFSIS_dims[RANK_soa_c2SFSIS];
    int soa_c2SFWET_dims[RANK_soa_c2SFWET];
    int soa_c2TBF_dims[RANK_soa_c2TBF];
    int soa_c2_sfcsiz3_dims[RANK_soa_c2_sfcsiz3];
    int soa_c2_sfcsiz4_dims[RANK_soa_c2_sfcsiz4];
    int soa_c2_sfgaex2_dims[RANK_soa_c2_sfgaex2];
    int wat_a1_dims[RANK_wat_a1];
    int wat_a2_dims[RANK_wat_a2];
    int wat_a3_dims[RANK_wat_a3];

    /* enter define mode */
    stat = nc_create("ref_camrun.nc", NC_CLOBBER|NC_NETCDF4, &ncid);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_put_att_text(ncid, NC_GLOBAL, "_Format", 1, "netCDF-4");
    check_err(stat,__LINE__,__FILE__);
    camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp = ncid;

    /* define dimensions */
    stat = nc_def_dim(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "lat", lat_len, &lat_dim);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_dim(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "lon", lon_len, &lon_dim);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_dim(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "slat", slat_len, &slat_dim);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_dim(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "slon", slon_len, &slon_dim);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_dim(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "lev", lev_len, &lev_dim);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_dim(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ilev", ilev_len, &ilev_dim);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_dim(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "isccp_prs", isccp_prs_len, &isccp_prs_dim);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_dim(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "isccp_tau", isccp_tau_len, &isccp_tau_dim);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_dim(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "isccp_prstau", isccp_prstau_len, &isccp_prstau_dim);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_dim(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "time", time_len, &time_dim);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_dim(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "tbnd", tbnd_len, &tbnd_dim);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_dim(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "chars", chars_len, &chars_dim);
    check_err(stat,__LINE__,__FILE__);

    /* define variables */

    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "P0", NC_DOUBLE, RANK_P0, 0, &P0_id);
    check_err(stat,__LINE__,__FILE__);

    lat_dims[0] = lat_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "lat", NC_DOUBLE, RANK_lat, lat_dims, &lat_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, lat_id, NC_CONTIGUOUS, NULL);
    check_err(stat,__LINE__,__FILE__);

    lon_dims[0] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "lon", NC_DOUBLE, RANK_lon, lon_dims, &lon_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, lon_id, NC_CONTIGUOUS, NULL);
    check_err(stat,__LINE__,__FILE__);

    slat_dims[0] = slat_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "slat", NC_DOUBLE, RANK_slat, slat_dims, &slat_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, slat_id, NC_CONTIGUOUS, NULL);
    check_err(stat,__LINE__,__FILE__);

    slon_dims[0] = slon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "slon", NC_DOUBLE, RANK_slon, slon_dims, &slon_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, slon_id, NC_CONTIGUOUS, NULL);
    check_err(stat,__LINE__,__FILE__);

    w_stag_dims[0] = slat_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "w_stag", NC_DOUBLE, RANK_w_stag, w_stag_dims, &w_stag_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, w_stag_id, NC_CONTIGUOUS, NULL);
    check_err(stat,__LINE__,__FILE__);

    lev_dims[0] = lev_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "lev", NC_DOUBLE, RANK_lev, lev_dims, &lev_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, lev_id, NC_CONTIGUOUS, NULL);
    check_err(stat,__LINE__,__FILE__);

    ilev_dims[0] = ilev_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ilev", NC_DOUBLE, RANK_ilev, ilev_dims, &ilev_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ilev_id, NC_CONTIGUOUS, NULL);
    check_err(stat,__LINE__,__FILE__);

    isccp_prs_dims[0] = isccp_prs_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "isccp_prs", NC_DOUBLE, RANK_isccp_prs, isccp_prs_dims, &isccp_prs_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, isccp_prs_id, NC_CONTIGUOUS, NULL);
    check_err(stat,__LINE__,__FILE__);

    isccp_tau_dims[0] = isccp_tau_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "isccp_tau", NC_DOUBLE, RANK_isccp_tau, isccp_tau_dims, &isccp_tau_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, isccp_tau_id, NC_CONTIGUOUS, NULL);
    check_err(stat,__LINE__,__FILE__);

    isccp_prstau_dims[0] = isccp_prstau_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "isccp_prstau", NC_DOUBLE, RANK_isccp_prstau, isccp_prstau_dims, &isccp_prstau_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, isccp_prstau_id, NC_CONTIGUOUS, NULL);
    check_err(stat,__LINE__,__FILE__);

    time_dims[0] = time_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "time", NC_DOUBLE, RANK_time, time_dims, &time_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, time_id, NC_CHUNKED, time_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    time_bnds_dims[0] = time_dim;
    time_bnds_dims[1] = tbnd_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "time_bnds", NC_DOUBLE, RANK_time_bnds, time_bnds_dims, &time_bnds_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, time_bnds_id, NC_CHUNKED, time_bnds_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    date_written_dims[0] = time_dim;
    date_written_dims[1] = chars_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "date_written", NC_CHAR, RANK_date_written, date_written_dims, &date_written_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, date_written_id, NC_CHUNKED, date_written_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    time_written_dims[0] = time_dim;
    time_written_dims[1] = chars_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "time_written", NC_CHAR, RANK_time_written, time_written_dims, &time_written_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, time_written_id, NC_CHUNKED, time_written_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ntrm", NC_INT, RANK_ntrm, 0, &ntrm_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_endian(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ntrm_id, NC_ENDIAN_BIG);
    check_err(stat,__LINE__,__FILE__);

    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ntrn", NC_INT, RANK_ntrn, 0, &ntrn_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_endian(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ntrn_id, NC_ENDIAN_BIG);
    check_err(stat,__LINE__,__FILE__);

    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ntrk", NC_INT, RANK_ntrk, 0, &ntrk_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_endian(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ntrk_id, NC_ENDIAN_BIG);
    check_err(stat,__LINE__,__FILE__);

    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ndbase", NC_INT, RANK_ndbase, 0, &ndbase_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_endian(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ndbase_id, NC_ENDIAN_BIG);
    check_err(stat,__LINE__,__FILE__);

    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "nsbase", NC_INT, RANK_nsbase, 0, &nsbase_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_endian(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, nsbase_id, NC_ENDIAN_BIG);
    check_err(stat,__LINE__,__FILE__);

    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "nbdate", NC_INT, RANK_nbdate, 0, &nbdate_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_endian(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, nbdate_id, NC_ENDIAN_BIG);
    check_err(stat,__LINE__,__FILE__);

    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "nbsec", NC_INT, RANK_nbsec, 0, &nbsec_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_endian(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, nbsec_id, NC_ENDIAN_BIG);
    check_err(stat,__LINE__,__FILE__);

    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "mdt", NC_INT, RANK_mdt, 0, &mdt_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_endian(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, mdt_id, NC_ENDIAN_BIG);
    check_err(stat,__LINE__,__FILE__);

    nlon_dims[0] = lat_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "nlon", NC_INT, RANK_nlon, nlon_dims, &nlon_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, nlon_id, NC_CONTIGUOUS, NULL);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_endian(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, nlon_id, NC_ENDIAN_BIG);
    check_err(stat,__LINE__,__FILE__);

    wnummax_dims[0] = lat_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "wnummax", NC_INT, RANK_wnummax, wnummax_dims, &wnummax_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, wnummax_id, NC_CONTIGUOUS, NULL);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_endian(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, wnummax_id, NC_ENDIAN_BIG);
    check_err(stat,__LINE__,__FILE__);

    hyai_dims[0] = ilev_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "hyai", NC_DOUBLE, RANK_hyai, hyai_dims, &hyai_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, hyai_id, NC_CONTIGUOUS, NULL);
    check_err(stat,__LINE__,__FILE__);

    hybi_dims[0] = ilev_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "hybi", NC_DOUBLE, RANK_hybi, hybi_dims, &hybi_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, hybi_id, NC_CONTIGUOUS, NULL);
    check_err(stat,__LINE__,__FILE__);

    hyam_dims[0] = lev_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "hyam", NC_DOUBLE, RANK_hyam, hyam_dims, &hyam_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, hyam_id, NC_CONTIGUOUS, NULL);
    check_err(stat,__LINE__,__FILE__);

    hybm_dims[0] = lev_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "hybm", NC_DOUBLE, RANK_hybm, hybm_dims, &hybm_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, hybm_id, NC_CONTIGUOUS, NULL);
    check_err(stat,__LINE__,__FILE__);

    gw_dims[0] = lat_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "gw", NC_DOUBLE, RANK_gw, gw_dims, &gw_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, gw_id, NC_CONTIGUOUS, NULL);
    check_err(stat,__LINE__,__FILE__);

    ndcur_dims[0] = time_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ndcur", NC_INT, RANK_ndcur, ndcur_dims, &ndcur_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ndcur_id, NC_CHUNKED, ndcur_chunksizes);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_endian(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ndcur_id, NC_ENDIAN_BIG);
    check_err(stat,__LINE__,__FILE__);

    nscur_dims[0] = time_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "nscur", NC_INT, RANK_nscur, nscur_dims, &nscur_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, nscur_id, NC_CHUNKED, nscur_chunksizes);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_endian(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, nscur_id, NC_ENDIAN_BIG);
    check_err(stat,__LINE__,__FILE__);

    date_dims[0] = time_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "date", NC_INT, RANK_date, date_dims, &date_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, date_id, NC_CHUNKED, date_chunksizes);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_endian(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, date_id, NC_ENDIAN_BIG);
    check_err(stat,__LINE__,__FILE__);

    co2vmr_dims[0] = time_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "co2vmr", NC_DOUBLE, RANK_co2vmr, co2vmr_dims, &co2vmr_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, co2vmr_id, NC_CHUNKED, co2vmr_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ch4vmr_dims[0] = time_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ch4vmr", NC_DOUBLE, RANK_ch4vmr, ch4vmr_dims, &ch4vmr_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ch4vmr_id, NC_CHUNKED, ch4vmr_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    n2ovmr_dims[0] = time_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "n2ovmr", NC_DOUBLE, RANK_n2ovmr, n2ovmr_dims, &n2ovmr_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, n2ovmr_id, NC_CHUNKED, n2ovmr_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    f11vmr_dims[0] = time_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "f11vmr", NC_DOUBLE, RANK_f11vmr, f11vmr_dims, &f11vmr_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, f11vmr_id, NC_CHUNKED, f11vmr_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    f12vmr_dims[0] = time_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "f12vmr", NC_DOUBLE, RANK_f12vmr, f12vmr_dims, &f12vmr_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, f12vmr_id, NC_CHUNKED, f12vmr_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    sol_tsi_dims[0] = time_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "sol_tsi", NC_DOUBLE, RANK_sol_tsi, sol_tsi_dims, &sol_tsi_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, sol_tsi_id, NC_CHUNKED, sol_tsi_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    datesec_dims[0] = time_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "datesec", NC_INT, RANK_datesec, datesec_dims, &datesec_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, datesec_id, NC_CHUNKED, datesec_chunksizes);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_endian(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, datesec_id, NC_ENDIAN_BIG);
    check_err(stat,__LINE__,__FILE__);

    nsteph_dims[0] = time_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "nsteph", NC_INT, RANK_nsteph, nsteph_dims, &nsteph_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, nsteph_id, NC_CHUNKED, nsteph_chunksizes);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_endian(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, nsteph_id, NC_ENDIAN_BIG);
    check_err(stat,__LINE__,__FILE__);

    ABSORB_dims[0] = time_dim;
    ABSORB_dims[1] = lev_dim;
    ABSORB_dims[2] = lat_dim;
    ABSORB_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ABSORB", NC_DOUBLE, RANK_ABSORB, ABSORB_dims, &ABSORB_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ABSORB_id, NC_CHUNKED, ABSORB_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AEROD_v_dims[0] = time_dim;
    AEROD_v_dims[1] = lat_dim;
    AEROD_v_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AEROD_v", NC_DOUBLE, RANK_AEROD_v, AEROD_v_dims, &AEROD_v_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AEROD_v_id, NC_CHUNKED, AEROD_v_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AODABS_dims[0] = time_dim;
    AODABS_dims[1] = lat_dim;
    AODABS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AODABS", NC_DOUBLE, RANK_AODABS, AODABS_dims, &AODABS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODABS_id, NC_CHUNKED, AODABS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AODDUST1_dims[0] = time_dim;
    AODDUST1_dims[1] = lat_dim;
    AODDUST1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AODDUST1", NC_DOUBLE, RANK_AODDUST1, AODDUST1_dims, &AODDUST1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODDUST1_id, NC_CHUNKED, AODDUST1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AODDUST2_dims[0] = time_dim;
    AODDUST2_dims[1] = lat_dim;
    AODDUST2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AODDUST2", NC_DOUBLE, RANK_AODDUST2, AODDUST2_dims, &AODDUST2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODDUST2_id, NC_CHUNKED, AODDUST2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AODDUST3_dims[0] = time_dim;
    AODDUST3_dims[1] = lat_dim;
    AODDUST3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AODDUST3", NC_DOUBLE, RANK_AODDUST3, AODDUST3_dims, &AODDUST3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODDUST3_id, NC_CHUNKED, AODDUST3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AODMODE1_dims[0] = time_dim;
    AODMODE1_dims[1] = lat_dim;
    AODMODE1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AODMODE1", NC_DOUBLE, RANK_AODMODE1, AODMODE1_dims, &AODMODE1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODMODE1_id, NC_CHUNKED, AODMODE1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AODMODE2_dims[0] = time_dim;
    AODMODE2_dims[1] = lat_dim;
    AODMODE2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AODMODE2", NC_DOUBLE, RANK_AODMODE2, AODMODE2_dims, &AODMODE2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODMODE2_id, NC_CHUNKED, AODMODE2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AODMODE3_dims[0] = time_dim;
    AODMODE3_dims[1] = lat_dim;
    AODMODE3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AODMODE3", NC_DOUBLE, RANK_AODMODE3, AODMODE3_dims, &AODMODE3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODMODE3_id, NC_CHUNKED, AODMODE3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AODVIS_dims[0] = time_dim;
    AODVIS_dims[1] = lat_dim;
    AODVIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AODVIS", NC_DOUBLE, RANK_AODVIS, AODVIS_dims, &AODVIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODVIS_id, NC_CHUNKED, AODVIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AQSO4_H2O2_dims[0] = time_dim;
    AQSO4_H2O2_dims[1] = lat_dim;
    AQSO4_H2O2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AQSO4_H2O2", NC_DOUBLE, RANK_AQSO4_H2O2, AQSO4_H2O2_dims, &AQSO4_H2O2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQSO4_H2O2_id, NC_CHUNKED, AQSO4_H2O2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AQSO4_O3_dims[0] = time_dim;
    AQSO4_O3_dims[1] = lat_dim;
    AQSO4_O3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AQSO4_O3", NC_DOUBLE, RANK_AQSO4_O3, AQSO4_O3_dims, &AQSO4_O3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQSO4_O3_id, NC_CHUNKED, AQSO4_O3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AQ_DMS_dims[0] = time_dim;
    AQ_DMS_dims[1] = lat_dim;
    AQ_DMS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AQ_DMS", NC_DOUBLE, RANK_AQ_DMS, AQ_DMS_dims, &AQ_DMS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_DMS_id, NC_CHUNKED, AQ_DMS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AQ_H2O2_dims[0] = time_dim;
    AQ_H2O2_dims[1] = lat_dim;
    AQ_H2O2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AQ_H2O2", NC_DOUBLE, RANK_AQ_H2O2, AQ_H2O2_dims, &AQ_H2O2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_H2O2_id, NC_CHUNKED, AQ_H2O2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AQ_H2SO4_dims[0] = time_dim;
    AQ_H2SO4_dims[1] = lat_dim;
    AQ_H2SO4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AQ_H2SO4", NC_DOUBLE, RANK_AQ_H2SO4, AQ_H2SO4_dims, &AQ_H2SO4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_H2SO4_id, NC_CHUNKED, AQ_H2SO4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AQ_SO2_dims[0] = time_dim;
    AQ_SO2_dims[1] = lat_dim;
    AQ_SO2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AQ_SO2", NC_DOUBLE, RANK_AQ_SO2, AQ_SO2_dims, &AQ_SO2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_SO2_id, NC_CHUNKED, AQ_SO2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AQ_SOAG_dims[0] = time_dim;
    AQ_SOAG_dims[1] = lat_dim;
    AQ_SOAG_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AQ_SOAG", NC_DOUBLE, RANK_AQ_SOAG, AQ_SOAG_dims, &AQ_SOAG_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_SOAG_id, NC_CHUNKED, AQ_SOAG_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AQ_bc_a1_dims[0] = time_dim;
    AQ_bc_a1_dims[1] = lat_dim;
    AQ_bc_a1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AQ_bc_a1", NC_DOUBLE, RANK_AQ_bc_a1, AQ_bc_a1_dims, &AQ_bc_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_bc_a1_id, NC_CHUNKED, AQ_bc_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AQ_dst_a1_dims[0] = time_dim;
    AQ_dst_a1_dims[1] = lat_dim;
    AQ_dst_a1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AQ_dst_a1", NC_DOUBLE, RANK_AQ_dst_a1, AQ_dst_a1_dims, &AQ_dst_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_dst_a1_id, NC_CHUNKED, AQ_dst_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AQ_dst_a3_dims[0] = time_dim;
    AQ_dst_a3_dims[1] = lat_dim;
    AQ_dst_a3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AQ_dst_a3", NC_DOUBLE, RANK_AQ_dst_a3, AQ_dst_a3_dims, &AQ_dst_a3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_dst_a3_id, NC_CHUNKED, AQ_dst_a3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AQ_ncl_a1_dims[0] = time_dim;
    AQ_ncl_a1_dims[1] = lat_dim;
    AQ_ncl_a1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AQ_ncl_a1", NC_DOUBLE, RANK_AQ_ncl_a1, AQ_ncl_a1_dims, &AQ_ncl_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_ncl_a1_id, NC_CHUNKED, AQ_ncl_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AQ_ncl_a2_dims[0] = time_dim;
    AQ_ncl_a2_dims[1] = lat_dim;
    AQ_ncl_a2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AQ_ncl_a2", NC_DOUBLE, RANK_AQ_ncl_a2, AQ_ncl_a2_dims, &AQ_ncl_a2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_ncl_a2_id, NC_CHUNKED, AQ_ncl_a2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AQ_ncl_a3_dims[0] = time_dim;
    AQ_ncl_a3_dims[1] = lat_dim;
    AQ_ncl_a3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AQ_ncl_a3", NC_DOUBLE, RANK_AQ_ncl_a3, AQ_ncl_a3_dims, &AQ_ncl_a3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_ncl_a3_id, NC_CHUNKED, AQ_ncl_a3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AQ_num_a1_dims[0] = time_dim;
    AQ_num_a1_dims[1] = lat_dim;
    AQ_num_a1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AQ_num_a1", NC_DOUBLE, RANK_AQ_num_a1, AQ_num_a1_dims, &AQ_num_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_num_a1_id, NC_CHUNKED, AQ_num_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AQ_num_a2_dims[0] = time_dim;
    AQ_num_a2_dims[1] = lat_dim;
    AQ_num_a2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AQ_num_a2", NC_DOUBLE, RANK_AQ_num_a2, AQ_num_a2_dims, &AQ_num_a2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_num_a2_id, NC_CHUNKED, AQ_num_a2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AQ_num_a3_dims[0] = time_dim;
    AQ_num_a3_dims[1] = lat_dim;
    AQ_num_a3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AQ_num_a3", NC_DOUBLE, RANK_AQ_num_a3, AQ_num_a3_dims, &AQ_num_a3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_num_a3_id, NC_CHUNKED, AQ_num_a3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AQ_pom_a1_dims[0] = time_dim;
    AQ_pom_a1_dims[1] = lat_dim;
    AQ_pom_a1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AQ_pom_a1", NC_DOUBLE, RANK_AQ_pom_a1, AQ_pom_a1_dims, &AQ_pom_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_pom_a1_id, NC_CHUNKED, AQ_pom_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AQ_so4_a1_dims[0] = time_dim;
    AQ_so4_a1_dims[1] = lat_dim;
    AQ_so4_a1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AQ_so4_a1", NC_DOUBLE, RANK_AQ_so4_a1, AQ_so4_a1_dims, &AQ_so4_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_so4_a1_id, NC_CHUNKED, AQ_so4_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AQ_so4_a2_dims[0] = time_dim;
    AQ_so4_a2_dims[1] = lat_dim;
    AQ_so4_a2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AQ_so4_a2", NC_DOUBLE, RANK_AQ_so4_a2, AQ_so4_a2_dims, &AQ_so4_a2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_so4_a2_id, NC_CHUNKED, AQ_so4_a2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AQ_so4_a3_dims[0] = time_dim;
    AQ_so4_a3_dims[1] = lat_dim;
    AQ_so4_a3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AQ_so4_a3", NC_DOUBLE, RANK_AQ_so4_a3, AQ_so4_a3_dims, &AQ_so4_a3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_so4_a3_id, NC_CHUNKED, AQ_so4_a3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AQ_soa_a1_dims[0] = time_dim;
    AQ_soa_a1_dims[1] = lat_dim;
    AQ_soa_a1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AQ_soa_a1", NC_DOUBLE, RANK_AQ_soa_a1, AQ_soa_a1_dims, &AQ_soa_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_soa_a1_id, NC_CHUNKED, AQ_soa_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    AQ_soa_a2_dims[0] = time_dim;
    AQ_soa_a2_dims[1] = lat_dim;
    AQ_soa_a2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "AQ_soa_a2", NC_DOUBLE, RANK_AQ_soa_a2, AQ_soa_a2_dims, &AQ_soa_a2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_soa_a2_id, NC_CHUNKED, AQ_soa_a2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    BPROD_dims[0] = time_dim;
    BPROD_dims[1] = ilev_dim;
    BPROD_dims[2] = lat_dim;
    BPROD_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "BPROD", NC_DOUBLE, RANK_BPROD, BPROD_dims, &BPROD_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, BPROD_id, NC_CHUNKED, BPROD_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    BURDEN1_dims[0] = time_dim;
    BURDEN1_dims[1] = lat_dim;
    BURDEN1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "BURDEN1", NC_DOUBLE, RANK_BURDEN1, BURDEN1_dims, &BURDEN1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, BURDEN1_id, NC_CHUNKED, BURDEN1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    BURDEN2_dims[0] = time_dim;
    BURDEN2_dims[1] = lat_dim;
    BURDEN2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "BURDEN2", NC_DOUBLE, RANK_BURDEN2, BURDEN2_dims, &BURDEN2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, BURDEN2_id, NC_CHUNKED, BURDEN2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    BURDEN3_dims[0] = time_dim;
    BURDEN3_dims[1] = lat_dim;
    BURDEN3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "BURDEN3", NC_DOUBLE, RANK_BURDEN3, BURDEN3_dims, &BURDEN3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, BURDEN3_id, NC_CHUNKED, BURDEN3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    CCN1_dims[0] = time_dim;
    CCN1_dims[1] = lev_dim;
    CCN1_dims[2] = lat_dim;
    CCN1_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "CCN1", NC_DOUBLE, RANK_CCN1, CCN1_dims, &CCN1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN1_id, NC_CHUNKED, CCN1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    CCN2_dims[0] = time_dim;
    CCN2_dims[1] = lev_dim;
    CCN2_dims[2] = lat_dim;
    CCN2_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "CCN2", NC_DOUBLE, RANK_CCN2, CCN2_dims, &CCN2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN2_id, NC_CHUNKED, CCN2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    CCN3_dims[0] = time_dim;
    CCN3_dims[1] = lev_dim;
    CCN3_dims[2] = lat_dim;
    CCN3_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "CCN3", NC_DOUBLE, RANK_CCN3, CCN3_dims, &CCN3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN3_id, NC_CHUNKED, CCN3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    CCN4_dims[0] = time_dim;
    CCN4_dims[1] = lev_dim;
    CCN4_dims[2] = lat_dim;
    CCN4_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "CCN4", NC_DOUBLE, RANK_CCN4, CCN4_dims, &CCN4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN4_id, NC_CHUNKED, CCN4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    CCN5_dims[0] = time_dim;
    CCN5_dims[1] = lev_dim;
    CCN5_dims[2] = lat_dim;
    CCN5_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "CCN5", NC_DOUBLE, RANK_CCN5, CCN5_dims, &CCN5_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN5_id, NC_CHUNKED, CCN5_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    CCN6_dims[0] = time_dim;
    CCN6_dims[1] = lev_dim;
    CCN6_dims[2] = lat_dim;
    CCN6_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "CCN6", NC_DOUBLE, RANK_CCN6, CCN6_dims, &CCN6_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN6_id, NC_CHUNKED, CCN6_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    CLDHGH_dims[0] = time_dim;
    CLDHGH_dims[1] = lat_dim;
    CLDHGH_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "CLDHGH", NC_DOUBLE, RANK_CLDHGH, CLDHGH_dims, &CLDHGH_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDHGH_id, NC_CHUNKED, CLDHGH_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    CLDICE_dims[0] = time_dim;
    CLDICE_dims[1] = lev_dim;
    CLDICE_dims[2] = lat_dim;
    CLDICE_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "CLDICE", NC_DOUBLE, RANK_CLDICE, CLDICE_dims, &CLDICE_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDICE_id, NC_CHUNKED, CLDICE_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    CLDLIQ_dims[0] = time_dim;
    CLDLIQ_dims[1] = lev_dim;
    CLDLIQ_dims[2] = lat_dim;
    CLDLIQ_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "CLDLIQ", NC_DOUBLE, RANK_CLDLIQ, CLDLIQ_dims, &CLDLIQ_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDLIQ_id, NC_CHUNKED, CLDLIQ_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    CLDLOW_dims[0] = time_dim;
    CLDLOW_dims[1] = lat_dim;
    CLDLOW_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "CLDLOW", NC_DOUBLE, RANK_CLDLOW, CLDLOW_dims, &CLDLOW_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDLOW_id, NC_CHUNKED, CLDLOW_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    CLDMED_dims[0] = time_dim;
    CLDMED_dims[1] = lat_dim;
    CLDMED_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "CLDMED", NC_DOUBLE, RANK_CLDMED, CLDMED_dims, &CLDMED_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDMED_id, NC_CHUNKED, CLDMED_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    CLDTOT_dims[0] = time_dim;
    CLDTOT_dims[1] = lat_dim;
    CLDTOT_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "CLDTOT", NC_DOUBLE, RANK_CLDTOT, CLDTOT_dims, &CLDTOT_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDTOT_id, NC_CHUNKED, CLDTOT_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    CLOUD_dims[0] = time_dim;
    CLOUD_dims[1] = lev_dim;
    CLOUD_dims[2] = lat_dim;
    CLOUD_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "CLOUD", NC_DOUBLE, RANK_CLOUD, CLOUD_dims, &CLOUD_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLOUD_id, NC_CHUNKED, CLOUD_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    CMFDQ_dims[0] = time_dim;
    CMFDQ_dims[1] = lev_dim;
    CMFDQ_dims[2] = lat_dim;
    CMFDQ_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "CMFDQ", NC_DOUBLE, RANK_CMFDQ, CMFDQ_dims, &CMFDQ_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CMFDQ_id, NC_CHUNKED, CMFDQ_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    CMFDQR_dims[0] = time_dim;
    CMFDQR_dims[1] = lev_dim;
    CMFDQR_dims[2] = lat_dim;
    CMFDQR_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "CMFDQR", NC_DOUBLE, RANK_CMFDQR, CMFDQR_dims, &CMFDQR_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CMFDQR_id, NC_CHUNKED, CMFDQR_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    CMFDT_dims[0] = time_dim;
    CMFDT_dims[1] = lev_dim;
    CMFDT_dims[2] = lat_dim;
    CMFDT_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "CMFDT", NC_DOUBLE, RANK_CMFDT, CMFDT_dims, &CMFDT_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CMFDT_id, NC_CHUNKED, CMFDT_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    CMFMC_dims[0] = time_dim;
    CMFMC_dims[1] = ilev_dim;
    CMFMC_dims[2] = lat_dim;
    CMFMC_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "CMFMC", NC_DOUBLE, RANK_CMFMC, CMFMC_dims, &CMFMC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CMFMC_id, NC_CHUNKED, CMFMC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    CMFMCDZM_dims[0] = time_dim;
    CMFMCDZM_dims[1] = ilev_dim;
    CMFMCDZM_dims[2] = lat_dim;
    CMFMCDZM_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "CMFMCDZM", NC_DOUBLE, RANK_CMFMCDZM, CMFMCDZM_dims, &CMFMCDZM_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CMFMCDZM_id, NC_CHUNKED, CMFMCDZM_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    CONCLD_dims[0] = time_dim;
    CONCLD_dims[1] = lev_dim;
    CONCLD_dims[2] = lat_dim;
    CONCLD_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "CONCLD", NC_DOUBLE, RANK_CONCLD, CONCLD_dims, &CONCLD_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CONCLD_id, NC_CHUNKED, CONCLD_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    DCQ_dims[0] = time_dim;
    DCQ_dims[1] = lev_dim;
    DCQ_dims[2] = lat_dim;
    DCQ_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "DCQ", NC_DOUBLE, RANK_DCQ, DCQ_dims, &DCQ_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DCQ_id, NC_CHUNKED, DCQ_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    DMS_dims[0] = time_dim;
    DMS_dims[1] = lev_dim;
    DMS_dims[2] = lat_dim;
    DMS_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "DMS", NC_DOUBLE, RANK_DMS, DMS_dims, &DMS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DMS_id, NC_CHUNKED, DMS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    DSTODXC_dims[0] = time_dim;
    DSTODXC_dims[1] = lat_dim;
    DSTODXC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "DSTODXC", NC_DOUBLE, RANK_DSTODXC, DSTODXC_dims, &DSTODXC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DSTODXC_id, NC_CHUNKED, DSTODXC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    DSTSFDRY_dims[0] = time_dim;
    DSTSFDRY_dims[1] = lat_dim;
    DSTSFDRY_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "DSTSFDRY", NC_DOUBLE, RANK_DSTSFDRY, DSTSFDRY_dims, &DSTSFDRY_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DSTSFDRY_id, NC_CHUNKED, DSTSFDRY_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    DSTSFMBL_dims[0] = time_dim;
    DSTSFMBL_dims[1] = lat_dim;
    DSTSFMBL_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "DSTSFMBL", NC_DOUBLE, RANK_DSTSFMBL, DSTSFMBL_dims, &DSTSFMBL_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DSTSFMBL_id, NC_CHUNKED, DSTSFMBL_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    DSTSFWET_dims[0] = time_dim;
    DSTSFWET_dims[1] = lat_dim;
    DSTSFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "DSTSFWET", NC_DOUBLE, RANK_DSTSFWET, DSTSFWET_dims, &DSTSFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DSTSFWET_id, NC_CHUNKED, DSTSFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    DTCOND_dims[0] = time_dim;
    DTCOND_dims[1] = lev_dim;
    DTCOND_dims[2] = lat_dim;
    DTCOND_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "DTCOND", NC_DOUBLE, RANK_DTCOND, DTCOND_dims, &DTCOND_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DTCOND_id, NC_CHUNKED, DTCOND_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    DTV_dims[0] = time_dim;
    DTV_dims[1] = lev_dim;
    DTV_dims[2] = lat_dim;
    DTV_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "DTV", NC_DOUBLE, RANK_DTV, DTV_dims, &DTV_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DTV_id, NC_CHUNKED, DTV_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    EXTINCT_dims[0] = time_dim;
    EXTINCT_dims[1] = lev_dim;
    EXTINCT_dims[2] = lat_dim;
    EXTINCT_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "EXTINCT", NC_DOUBLE, RANK_EXTINCT, EXTINCT_dims, &EXTINCT_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, EXTINCT_id, NC_CHUNKED, EXTINCT_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    FICE_dims[0] = time_dim;
    FICE_dims[1] = lev_dim;
    FICE_dims[2] = lat_dim;
    FICE_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "FICE", NC_DOUBLE, RANK_FICE, FICE_dims, &FICE_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FICE_id, NC_CHUNKED, FICE_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    FLDS_dims[0] = time_dim;
    FLDS_dims[1] = lat_dim;
    FLDS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "FLDS", NC_DOUBLE, RANK_FLDS, FLDS_dims, &FLDS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLDS_id, NC_CHUNKED, FLDS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    FLNS_dims[0] = time_dim;
    FLNS_dims[1] = lat_dim;
    FLNS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "FLNS", NC_DOUBLE, RANK_FLNS, FLNS_dims, &FLNS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLNS_id, NC_CHUNKED, FLNS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    FLNSC_dims[0] = time_dim;
    FLNSC_dims[1] = lat_dim;
    FLNSC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "FLNSC", NC_DOUBLE, RANK_FLNSC, FLNSC_dims, &FLNSC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLNSC_id, NC_CHUNKED, FLNSC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    FLNT_dims[0] = time_dim;
    FLNT_dims[1] = lat_dim;
    FLNT_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "FLNT", NC_DOUBLE, RANK_FLNT, FLNT_dims, &FLNT_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLNT_id, NC_CHUNKED, FLNT_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    FLNTC_dims[0] = time_dim;
    FLNTC_dims[1] = lat_dim;
    FLNTC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "FLNTC", NC_DOUBLE, RANK_FLNTC, FLNTC_dims, &FLNTC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLNTC_id, NC_CHUNKED, FLNTC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    FLUT_dims[0] = time_dim;
    FLUT_dims[1] = lat_dim;
    FLUT_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "FLUT", NC_DOUBLE, RANK_FLUT, FLUT_dims, &FLUT_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLUT_id, NC_CHUNKED, FLUT_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    FLUTC_dims[0] = time_dim;
    FLUTC_dims[1] = lat_dim;
    FLUTC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "FLUTC", NC_DOUBLE, RANK_FLUTC, FLUTC_dims, &FLUTC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLUTC_id, NC_CHUNKED, FLUTC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    FREQSH_dims[0] = time_dim;
    FREQSH_dims[1] = lat_dim;
    FREQSH_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "FREQSH", NC_DOUBLE, RANK_FREQSH, FREQSH_dims, &FREQSH_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FREQSH_id, NC_CHUNKED, FREQSH_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    FREQZM_dims[0] = time_dim;
    FREQZM_dims[1] = lat_dim;
    FREQZM_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "FREQZM", NC_DOUBLE, RANK_FREQZM, FREQZM_dims, &FREQZM_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FREQZM_id, NC_CHUNKED, FREQZM_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    FSDS_dims[0] = time_dim;
    FSDS_dims[1] = lat_dim;
    FSDS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "FSDS", NC_DOUBLE, RANK_FSDS, FSDS_dims, &FSDS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSDS_id, NC_CHUNKED, FSDS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    FSDSC_dims[0] = time_dim;
    FSDSC_dims[1] = lat_dim;
    FSDSC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "FSDSC", NC_DOUBLE, RANK_FSDSC, FSDSC_dims, &FSDSC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSDSC_id, NC_CHUNKED, FSDSC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    FSNS_dims[0] = time_dim;
    FSNS_dims[1] = lat_dim;
    FSNS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "FSNS", NC_DOUBLE, RANK_FSNS, FSNS_dims, &FSNS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNS_id, NC_CHUNKED, FSNS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    FSNSC_dims[0] = time_dim;
    FSNSC_dims[1] = lat_dim;
    FSNSC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "FSNSC", NC_DOUBLE, RANK_FSNSC, FSNSC_dims, &FSNSC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNSC_id, NC_CHUNKED, FSNSC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    FSNT_dims[0] = time_dim;
    FSNT_dims[1] = lat_dim;
    FSNT_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "FSNT", NC_DOUBLE, RANK_FSNT, FSNT_dims, &FSNT_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNT_id, NC_CHUNKED, FSNT_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    FSNTC_dims[0] = time_dim;
    FSNTC_dims[1] = lat_dim;
    FSNTC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "FSNTC", NC_DOUBLE, RANK_FSNTC, FSNTC_dims, &FSNTC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNTC_id, NC_CHUNKED, FSNTC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    FSNTOA_dims[0] = time_dim;
    FSNTOA_dims[1] = lat_dim;
    FSNTOA_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "FSNTOA", NC_DOUBLE, RANK_FSNTOA, FSNTOA_dims, &FSNTOA_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNTOA_id, NC_CHUNKED, FSNTOA_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    FSNTOAC_dims[0] = time_dim;
    FSNTOAC_dims[1] = lat_dim;
    FSNTOAC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "FSNTOAC", NC_DOUBLE, RANK_FSNTOAC, FSNTOAC_dims, &FSNTOAC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNTOAC_id, NC_CHUNKED, FSNTOAC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    FSUTOA_dims[0] = time_dim;
    FSUTOA_dims[1] = lat_dim;
    FSUTOA_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "FSUTOA", NC_DOUBLE, RANK_FSUTOA, FSUTOA_dims, &FSUTOA_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSUTOA_id, NC_CHUNKED, FSUTOA_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    GS_DMS_dims[0] = time_dim;
    GS_DMS_dims[1] = lat_dim;
    GS_DMS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "GS_DMS", NC_DOUBLE, RANK_GS_DMS, GS_DMS_dims, &GS_DMS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_DMS_id, NC_CHUNKED, GS_DMS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    GS_H2O2_dims[0] = time_dim;
    GS_H2O2_dims[1] = lat_dim;
    GS_H2O2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "GS_H2O2", NC_DOUBLE, RANK_GS_H2O2, GS_H2O2_dims, &GS_H2O2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_H2O2_id, NC_CHUNKED, GS_H2O2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    GS_H2SO4_dims[0] = time_dim;
    GS_H2SO4_dims[1] = lat_dim;
    GS_H2SO4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "GS_H2SO4", NC_DOUBLE, RANK_GS_H2SO4, GS_H2SO4_dims, &GS_H2SO4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_H2SO4_id, NC_CHUNKED, GS_H2SO4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    GS_SO2_dims[0] = time_dim;
    GS_SO2_dims[1] = lat_dim;
    GS_SO2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "GS_SO2", NC_DOUBLE, RANK_GS_SO2, GS_SO2_dims, &GS_SO2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_SO2_id, NC_CHUNKED, GS_SO2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    GS_SOAG_dims[0] = time_dim;
    GS_SOAG_dims[1] = lat_dim;
    GS_SOAG_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "GS_SOAG", NC_DOUBLE, RANK_GS_SOAG, GS_SOAG_dims, &GS_SOAG_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_SOAG_id, NC_CHUNKED, GS_SOAG_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    GS_bc_a1_dims[0] = time_dim;
    GS_bc_a1_dims[1] = lat_dim;
    GS_bc_a1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "GS_bc_a1", NC_DOUBLE, RANK_GS_bc_a1, GS_bc_a1_dims, &GS_bc_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_bc_a1_id, NC_CHUNKED, GS_bc_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    GS_dst_a1_dims[0] = time_dim;
    GS_dst_a1_dims[1] = lat_dim;
    GS_dst_a1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "GS_dst_a1", NC_DOUBLE, RANK_GS_dst_a1, GS_dst_a1_dims, &GS_dst_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_dst_a1_id, NC_CHUNKED, GS_dst_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    GS_dst_a3_dims[0] = time_dim;
    GS_dst_a3_dims[1] = lat_dim;
    GS_dst_a3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "GS_dst_a3", NC_DOUBLE, RANK_GS_dst_a3, GS_dst_a3_dims, &GS_dst_a3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_dst_a3_id, NC_CHUNKED, GS_dst_a3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    GS_ncl_a1_dims[0] = time_dim;
    GS_ncl_a1_dims[1] = lat_dim;
    GS_ncl_a1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "GS_ncl_a1", NC_DOUBLE, RANK_GS_ncl_a1, GS_ncl_a1_dims, &GS_ncl_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_ncl_a1_id, NC_CHUNKED, GS_ncl_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    GS_ncl_a2_dims[0] = time_dim;
    GS_ncl_a2_dims[1] = lat_dim;
    GS_ncl_a2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "GS_ncl_a2", NC_DOUBLE, RANK_GS_ncl_a2, GS_ncl_a2_dims, &GS_ncl_a2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_ncl_a2_id, NC_CHUNKED, GS_ncl_a2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    GS_ncl_a3_dims[0] = time_dim;
    GS_ncl_a3_dims[1] = lat_dim;
    GS_ncl_a3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "GS_ncl_a3", NC_DOUBLE, RANK_GS_ncl_a3, GS_ncl_a3_dims, &GS_ncl_a3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_ncl_a3_id, NC_CHUNKED, GS_ncl_a3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    GS_num_a1_dims[0] = time_dim;
    GS_num_a1_dims[1] = lat_dim;
    GS_num_a1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "GS_num_a1", NC_DOUBLE, RANK_GS_num_a1, GS_num_a1_dims, &GS_num_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_num_a1_id, NC_CHUNKED, GS_num_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    GS_num_a2_dims[0] = time_dim;
    GS_num_a2_dims[1] = lat_dim;
    GS_num_a2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "GS_num_a2", NC_DOUBLE, RANK_GS_num_a2, GS_num_a2_dims, &GS_num_a2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_num_a2_id, NC_CHUNKED, GS_num_a2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    GS_num_a3_dims[0] = time_dim;
    GS_num_a3_dims[1] = lat_dim;
    GS_num_a3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "GS_num_a3", NC_DOUBLE, RANK_GS_num_a3, GS_num_a3_dims, &GS_num_a3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_num_a3_id, NC_CHUNKED, GS_num_a3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    GS_pom_a1_dims[0] = time_dim;
    GS_pom_a1_dims[1] = lat_dim;
    GS_pom_a1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "GS_pom_a1", NC_DOUBLE, RANK_GS_pom_a1, GS_pom_a1_dims, &GS_pom_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_pom_a1_id, NC_CHUNKED, GS_pom_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    GS_so4_a1_dims[0] = time_dim;
    GS_so4_a1_dims[1] = lat_dim;
    GS_so4_a1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "GS_so4_a1", NC_DOUBLE, RANK_GS_so4_a1, GS_so4_a1_dims, &GS_so4_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_so4_a1_id, NC_CHUNKED, GS_so4_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    GS_so4_a2_dims[0] = time_dim;
    GS_so4_a2_dims[1] = lat_dim;
    GS_so4_a2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "GS_so4_a2", NC_DOUBLE, RANK_GS_so4_a2, GS_so4_a2_dims, &GS_so4_a2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_so4_a2_id, NC_CHUNKED, GS_so4_a2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    GS_so4_a3_dims[0] = time_dim;
    GS_so4_a3_dims[1] = lat_dim;
    GS_so4_a3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "GS_so4_a3", NC_DOUBLE, RANK_GS_so4_a3, GS_so4_a3_dims, &GS_so4_a3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_so4_a3_id, NC_CHUNKED, GS_so4_a3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    GS_soa_a1_dims[0] = time_dim;
    GS_soa_a1_dims[1] = lat_dim;
    GS_soa_a1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "GS_soa_a1", NC_DOUBLE, RANK_GS_soa_a1, GS_soa_a1_dims, &GS_soa_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_soa_a1_id, NC_CHUNKED, GS_soa_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    GS_soa_a2_dims[0] = time_dim;
    GS_soa_a2_dims[1] = lat_dim;
    GS_soa_a2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "GS_soa_a2", NC_DOUBLE, RANK_GS_soa_a2, GS_soa_a2_dims, &GS_soa_a2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_soa_a2_id, NC_CHUNKED, GS_soa_a2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    H2O2_dims[0] = time_dim;
    H2O2_dims[1] = lev_dim;
    H2O2_dims[2] = lat_dim;
    H2O2_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "H2O2", NC_DOUBLE, RANK_H2O2, H2O2_dims, &H2O2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, H2O2_id, NC_CHUNKED, H2O2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    H2SO4_dims[0] = time_dim;
    H2SO4_dims[1] = lev_dim;
    H2SO4_dims[2] = lat_dim;
    H2SO4_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "H2SO4", NC_DOUBLE, RANK_H2SO4, H2SO4_dims, &H2SO4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, H2SO4_id, NC_CHUNKED, H2SO4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    H2SO4_sfgaex1_dims[0] = time_dim;
    H2SO4_sfgaex1_dims[1] = lat_dim;
    H2SO4_sfgaex1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "H2SO4_sfgaex1", NC_DOUBLE, RANK_H2SO4_sfgaex1, H2SO4_sfgaex1_dims, &H2SO4_sfgaex1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, H2SO4_sfgaex1_id, NC_CHUNKED, H2SO4_sfgaex1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    H2SO4_sfnnuc1_dims[0] = time_dim;
    H2SO4_sfnnuc1_dims[1] = lat_dim;
    H2SO4_sfnnuc1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "H2SO4_sfnnuc1", NC_DOUBLE, RANK_H2SO4_sfnnuc1, H2SO4_sfnnuc1_dims, &H2SO4_sfnnuc1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, H2SO4_sfnnuc1_id, NC_CHUNKED, H2SO4_sfnnuc1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ICEFRAC_dims[0] = time_dim;
    ICEFRAC_dims[1] = lat_dim;
    ICEFRAC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ICEFRAC", NC_DOUBLE, RANK_ICEFRAC, ICEFRAC_dims, &ICEFRAC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ICEFRAC_id, NC_CHUNKED, ICEFRAC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ICIMR_dims[0] = time_dim;
    ICIMR_dims[1] = lev_dim;
    ICIMR_dims[2] = lat_dim;
    ICIMR_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ICIMR", NC_DOUBLE, RANK_ICIMR, ICIMR_dims, &ICIMR_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ICIMR_id, NC_CHUNKED, ICIMR_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ICWMR_dims[0] = time_dim;
    ICWMR_dims[1] = lev_dim;
    ICWMR_dims[2] = lat_dim;
    ICWMR_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ICWMR", NC_DOUBLE, RANK_ICWMR, ICWMR_dims, &ICWMR_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ICWMR_id, NC_CHUNKED, ICWMR_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    KVH_dims[0] = time_dim;
    KVH_dims[1] = ilev_dim;
    KVH_dims[2] = lat_dim;
    KVH_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "KVH", NC_DOUBLE, RANK_KVH, KVH_dims, &KVH_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, KVH_id, NC_CHUNKED, KVH_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    KVM_dims[0] = time_dim;
    KVM_dims[1] = ilev_dim;
    KVM_dims[2] = lat_dim;
    KVM_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "KVM", NC_DOUBLE, RANK_KVM, KVM_dims, &KVM_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, KVM_id, NC_CHUNKED, KVM_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    LANDFRAC_dims[0] = time_dim;
    LANDFRAC_dims[1] = lat_dim;
    LANDFRAC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "LANDFRAC", NC_DOUBLE, RANK_LANDFRAC, LANDFRAC_dims, &LANDFRAC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, LANDFRAC_id, NC_CHUNKED, LANDFRAC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    LCLOUD_dims[0] = time_dim;
    LCLOUD_dims[1] = lev_dim;
    LCLOUD_dims[2] = lat_dim;
    LCLOUD_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "LCLOUD", NC_DOUBLE, RANK_LCLOUD, LCLOUD_dims, &LCLOUD_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, LCLOUD_id, NC_CHUNKED, LCLOUD_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    LHFLX_dims[0] = time_dim;
    LHFLX_dims[1] = lat_dim;
    LHFLX_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "LHFLX", NC_DOUBLE, RANK_LHFLX, LHFLX_dims, &LHFLX_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, LHFLX_id, NC_CHUNKED, LHFLX_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    LND_MBL_dims[0] = time_dim;
    LND_MBL_dims[1] = lat_dim;
    LND_MBL_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "LND_MBL", NC_DOUBLE, RANK_LND_MBL, LND_MBL_dims, &LND_MBL_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, LND_MBL_id, NC_CHUNKED, LND_MBL_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    LWCF_dims[0] = time_dim;
    LWCF_dims[1] = lat_dim;
    LWCF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "LWCF", NC_DOUBLE, RANK_LWCF, LWCF_dims, &LWCF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, LWCF_id, NC_CHUNKED, LWCF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    NDROPCOL_dims[0] = time_dim;
    NDROPCOL_dims[1] = lat_dim;
    NDROPCOL_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "NDROPCOL", NC_DOUBLE, RANK_NDROPCOL, NDROPCOL_dims, &NDROPCOL_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NDROPCOL_id, NC_CHUNKED, NDROPCOL_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    NDROPMIX_dims[0] = time_dim;
    NDROPMIX_dims[1] = lev_dim;
    NDROPMIX_dims[2] = lat_dim;
    NDROPMIX_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "NDROPMIX", NC_DOUBLE, RANK_NDROPMIX, NDROPMIX_dims, &NDROPMIX_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NDROPMIX_id, NC_CHUNKED, NDROPMIX_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    NDROPSNK_dims[0] = time_dim;
    NDROPSNK_dims[1] = lev_dim;
    NDROPSNK_dims[2] = lat_dim;
    NDROPSNK_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "NDROPSNK", NC_DOUBLE, RANK_NDROPSNK, NDROPSNK_dims, &NDROPSNK_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NDROPSNK_id, NC_CHUNKED, NDROPSNK_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    NDROPSRC_dims[0] = time_dim;
    NDROPSRC_dims[1] = lev_dim;
    NDROPSRC_dims[2] = lat_dim;
    NDROPSRC_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "NDROPSRC", NC_DOUBLE, RANK_NDROPSRC, NDROPSRC_dims, &NDROPSRC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NDROPSRC_id, NC_CHUNKED, NDROPSRC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    NUMICE_dims[0] = time_dim;
    NUMICE_dims[1] = lev_dim;
    NUMICE_dims[2] = lat_dim;
    NUMICE_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "NUMICE", NC_DOUBLE, RANK_NUMICE, NUMICE_dims, &NUMICE_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NUMICE_id, NC_CHUNKED, NUMICE_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    NUMLIQ_dims[0] = time_dim;
    NUMLIQ_dims[1] = lev_dim;
    NUMLIQ_dims[2] = lat_dim;
    NUMLIQ_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "NUMLIQ", NC_DOUBLE, RANK_NUMLIQ, NUMLIQ_dims, &NUMLIQ_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NUMLIQ_id, NC_CHUNKED, NUMLIQ_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    OCNFRAC_dims[0] = time_dim;
    OCNFRAC_dims[1] = lat_dim;
    OCNFRAC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "OCNFRAC", NC_DOUBLE, RANK_OCNFRAC, OCNFRAC_dims, &OCNFRAC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, OCNFRAC_id, NC_CHUNKED, OCNFRAC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ODV_bc_a1_dims[0] = time_dim;
    ODV_bc_a1_dims[1] = lat_dim;
    ODV_bc_a1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ODV_bc_a1", NC_DOUBLE, RANK_ODV_bc_a1, ODV_bc_a1_dims, &ODV_bc_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_bc_a1_id, NC_CHUNKED, ODV_bc_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ODV_dst_a1_dims[0] = time_dim;
    ODV_dst_a1_dims[1] = lat_dim;
    ODV_dst_a1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ODV_dst_a1", NC_DOUBLE, RANK_ODV_dst_a1, ODV_dst_a1_dims, &ODV_dst_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_dst_a1_id, NC_CHUNKED, ODV_dst_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ODV_dst_a3_dims[0] = time_dim;
    ODV_dst_a3_dims[1] = lat_dim;
    ODV_dst_a3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ODV_dst_a3", NC_DOUBLE, RANK_ODV_dst_a3, ODV_dst_a3_dims, &ODV_dst_a3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_dst_a3_id, NC_CHUNKED, ODV_dst_a3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ODV_ncl_a1_dims[0] = time_dim;
    ODV_ncl_a1_dims[1] = lat_dim;
    ODV_ncl_a1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ODV_ncl_a1", NC_DOUBLE, RANK_ODV_ncl_a1, ODV_ncl_a1_dims, &ODV_ncl_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_ncl_a1_id, NC_CHUNKED, ODV_ncl_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ODV_ncl_a3_dims[0] = time_dim;
    ODV_ncl_a3_dims[1] = lat_dim;
    ODV_ncl_a3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ODV_ncl_a3", NC_DOUBLE, RANK_ODV_ncl_a3, ODV_ncl_a3_dims, &ODV_ncl_a3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_ncl_a3_id, NC_CHUNKED, ODV_ncl_a3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ODV_pom_a1_dims[0] = time_dim;
    ODV_pom_a1_dims[1] = lat_dim;
    ODV_pom_a1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ODV_pom_a1", NC_DOUBLE, RANK_ODV_pom_a1, ODV_pom_a1_dims, &ODV_pom_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_pom_a1_id, NC_CHUNKED, ODV_pom_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ODV_so4_a1_dims[0] = time_dim;
    ODV_so4_a1_dims[1] = lat_dim;
    ODV_so4_a1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ODV_so4_a1", NC_DOUBLE, RANK_ODV_so4_a1, ODV_so4_a1_dims, &ODV_so4_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_so4_a1_id, NC_CHUNKED, ODV_so4_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ODV_soa_a1_dims[0] = time_dim;
    ODV_soa_a1_dims[1] = lat_dim;
    ODV_soa_a1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ODV_soa_a1", NC_DOUBLE, RANK_ODV_soa_a1, ODV_soa_a1_dims, &ODV_soa_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_soa_a1_id, NC_CHUNKED, ODV_soa_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    OMEGA_dims[0] = time_dim;
    OMEGA_dims[1] = lev_dim;
    OMEGA_dims[2] = lat_dim;
    OMEGA_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "OMEGA", NC_DOUBLE, RANK_OMEGA, OMEGA_dims, &OMEGA_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, OMEGA_id, NC_CHUNKED, OMEGA_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    OMEGAT_dims[0] = time_dim;
    OMEGAT_dims[1] = lev_dim;
    OMEGAT_dims[2] = lat_dim;
    OMEGAT_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "OMEGAT", NC_DOUBLE, RANK_OMEGAT, OMEGAT_dims, &OMEGAT_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, OMEGAT_id, NC_CHUNKED, OMEGAT_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ORO_dims[0] = time_dim;
    ORO_dims[1] = lat_dim;
    ORO_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ORO", NC_DOUBLE, RANK_ORO, ORO_dims, &ORO_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ORO_id, NC_CHUNKED, ORO_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    PBLH_dims[0] = time_dim;
    PBLH_dims[1] = lat_dim;
    PBLH_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "PBLH", NC_DOUBLE, RANK_PBLH, PBLH_dims, &PBLH_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PBLH_id, NC_CHUNKED, PBLH_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    PCONVB_dims[0] = time_dim;
    PCONVB_dims[1] = lat_dim;
    PCONVB_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "PCONVB", NC_DOUBLE, RANK_PCONVB, PCONVB_dims, &PCONVB_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PCONVB_id, NC_CHUNKED, PCONVB_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    PCONVT_dims[0] = time_dim;
    PCONVT_dims[1] = lat_dim;
    PCONVT_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "PCONVT", NC_DOUBLE, RANK_PCONVT, PCONVT_dims, &PCONVT_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PCONVT_id, NC_CHUNKED, PCONVT_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    PHIS_dims[0] = time_dim;
    PHIS_dims[1] = lat_dim;
    PHIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "PHIS", NC_DOUBLE, RANK_PHIS, PHIS_dims, &PHIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PHIS_id, NC_CHUNKED, PHIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    PRECC_dims[0] = time_dim;
    PRECC_dims[1] = lat_dim;
    PRECC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "PRECC", NC_DOUBLE, RANK_PRECC, PRECC_dims, &PRECC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECC_id, NC_CHUNKED, PRECC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    PRECCDZM_dims[0] = time_dim;
    PRECCDZM_dims[1] = lat_dim;
    PRECCDZM_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "PRECCDZM", NC_DOUBLE, RANK_PRECCDZM, PRECCDZM_dims, &PRECCDZM_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECCDZM_id, NC_CHUNKED, PRECCDZM_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    PRECL_dims[0] = time_dim;
    PRECL_dims[1] = lat_dim;
    PRECL_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "PRECL", NC_DOUBLE, RANK_PRECL, PRECL_dims, &PRECL_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECL_id, NC_CHUNKED, PRECL_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    PRECSC_dims[0] = time_dim;
    PRECSC_dims[1] = lat_dim;
    PRECSC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "PRECSC", NC_DOUBLE, RANK_PRECSC, PRECSC_dims, &PRECSC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECSC_id, NC_CHUNKED, PRECSC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    PRECSH_dims[0] = time_dim;
    PRECSH_dims[1] = lat_dim;
    PRECSH_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "PRECSH", NC_DOUBLE, RANK_PRECSH, PRECSH_dims, &PRECSH_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECSH_id, NC_CHUNKED, PRECSH_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    PRECSL_dims[0] = time_dim;
    PRECSL_dims[1] = lat_dim;
    PRECSL_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "PRECSL", NC_DOUBLE, RANK_PRECSL, PRECSL_dims, &PRECSL_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECSL_id, NC_CHUNKED, PRECSL_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    PRECT_dims[0] = time_dim;
    PRECT_dims[1] = lat_dim;
    PRECT_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "PRECT", NC_DOUBLE, RANK_PRECT, PRECT_dims, &PRECT_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECT_id, NC_CHUNKED, PRECT_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    PS_dims[0] = time_dim;
    PS_dims[1] = lat_dim;
    PS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "PS", NC_DOUBLE, RANK_PS, PS_dims, &PS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PS_id, NC_CHUNKED, PS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    PSL_dims[0] = time_dim;
    PSL_dims[1] = lat_dim;
    PSL_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "PSL", NC_DOUBLE, RANK_PSL, PSL_dims, &PSL_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PSL_id, NC_CHUNKED, PSL_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    Q_dims[0] = time_dim;
    Q_dims[1] = lev_dim;
    Q_dims[2] = lat_dim;
    Q_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "Q", NC_DOUBLE, RANK_Q, Q_dims, &Q_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, Q_id, NC_CHUNKED, Q_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    QC_dims[0] = time_dim;
    QC_dims[1] = lev_dim;
    QC_dims[2] = lat_dim;
    QC_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "QC", NC_DOUBLE, RANK_QC, QC_dims, &QC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QC_id, NC_CHUNKED, QC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    QFLX_dims[0] = time_dim;
    QFLX_dims[1] = lat_dim;
    QFLX_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "QFLX", NC_DOUBLE, RANK_QFLX, QFLX_dims, &QFLX_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QFLX_id, NC_CHUNKED, QFLX_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    QREFHT_dims[0] = time_dim;
    QREFHT_dims[1] = lat_dim;
    QREFHT_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "QREFHT", NC_DOUBLE, RANK_QREFHT, QREFHT_dims, &QREFHT_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QREFHT_id, NC_CHUNKED, QREFHT_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    QRL_dims[0] = time_dim;
    QRL_dims[1] = lev_dim;
    QRL_dims[2] = lat_dim;
    QRL_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "QRL", NC_DOUBLE, RANK_QRL, QRL_dims, &QRL_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QRL_id, NC_CHUNKED, QRL_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    QRS_dims[0] = time_dim;
    QRS_dims[1] = lev_dim;
    QRS_dims[2] = lat_dim;
    QRS_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "QRS", NC_DOUBLE, RANK_QRS, QRS_dims, &QRS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QRS_id, NC_CHUNKED, QRS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    QT_dims[0] = time_dim;
    QT_dims[1] = lev_dim;
    QT_dims[2] = lat_dim;
    QT_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "QT", NC_DOUBLE, RANK_QT, QT_dims, &QT_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QT_id, NC_CHUNKED, QT_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    QTFLX_dims[0] = time_dim;
    QTFLX_dims[1] = ilev_dim;
    QTFLX_dims[2] = lat_dim;
    QTFLX_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "QTFLX", NC_DOUBLE, RANK_QTFLX, QTFLX_dims, &QTFLX_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QTFLX_id, NC_CHUNKED, QTFLX_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    RAM1_dims[0] = time_dim;
    RAM1_dims[1] = lat_dim;
    RAM1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "RAM1", NC_DOUBLE, RANK_RAM1, RAM1_dims, &RAM1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, RAM1_id, NC_CHUNKED, RAM1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    RELHUM_dims[0] = time_dim;
    RELHUM_dims[1] = lev_dim;
    RELHUM_dims[2] = lat_dim;
    RELHUM_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "RELHUM", NC_DOUBLE, RANK_RELHUM, RELHUM_dims, &RELHUM_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, RELHUM_id, NC_CHUNKED, RELHUM_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    RHREFHT_dims[0] = time_dim;
    RHREFHT_dims[1] = lat_dim;
    RHREFHT_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "RHREFHT", NC_DOUBLE, RANK_RHREFHT, RHREFHT_dims, &RHREFHT_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, RHREFHT_id, NC_CHUNKED, RHREFHT_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SFCLDICE_dims[0] = time_dim;
    SFCLDICE_dims[1] = lat_dim;
    SFCLDICE_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SFCLDICE", NC_DOUBLE, RANK_SFCLDICE, SFCLDICE_dims, &SFCLDICE_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SFCLDICE_id, NC_CHUNKED, SFCLDICE_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SFCLDLIQ_dims[0] = time_dim;
    SFCLDLIQ_dims[1] = lat_dim;
    SFCLDLIQ_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SFCLDLIQ", NC_DOUBLE, RANK_SFCLDLIQ, SFCLDLIQ_dims, &SFCLDLIQ_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SFCLDLIQ_id, NC_CHUNKED, SFCLDLIQ_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SFI_dims[0] = time_dim;
    SFI_dims[1] = ilev_dim;
    SFI_dims[2] = lat_dim;
    SFI_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SFI", NC_DOUBLE, RANK_SFI, SFI_dims, &SFI_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SFI_id, NC_CHUNKED, SFI_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SFNUMICE_dims[0] = time_dim;
    SFNUMICE_dims[1] = lat_dim;
    SFNUMICE_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SFNUMICE", NC_DOUBLE, RANK_SFNUMICE, SFNUMICE_dims, &SFNUMICE_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SFNUMICE_id, NC_CHUNKED, SFNUMICE_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SFNUMLIQ_dims[0] = time_dim;
    SFNUMLIQ_dims[1] = lat_dim;
    SFNUMLIQ_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SFNUMLIQ", NC_DOUBLE, RANK_SFNUMLIQ, SFNUMLIQ_dims, &SFNUMLIQ_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SFNUMLIQ_id, NC_CHUNKED, SFNUMLIQ_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SHFLX_dims[0] = time_dim;
    SHFLX_dims[1] = lat_dim;
    SHFLX_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SHFLX", NC_DOUBLE, RANK_SHFLX, SHFLX_dims, &SHFLX_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SHFLX_id, NC_CHUNKED, SHFLX_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SL_dims[0] = time_dim;
    SL_dims[1] = lev_dim;
    SL_dims[2] = lat_dim;
    SL_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SL", NC_DOUBLE, RANK_SL, SL_dims, &SL_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SL_id, NC_CHUNKED, SL_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SLFLX_dims[0] = time_dim;
    SLFLX_dims[1] = ilev_dim;
    SLFLX_dims[2] = lat_dim;
    SLFLX_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SLFLX", NC_DOUBLE, RANK_SLFLX, SLFLX_dims, &SLFLX_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SLFLX_id, NC_CHUNKED, SLFLX_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SLV_dims[0] = time_dim;
    SLV_dims[1] = lev_dim;
    SLV_dims[2] = lat_dim;
    SLV_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SLV", NC_DOUBLE, RANK_SLV, SLV_dims, &SLV_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SLV_id, NC_CHUNKED, SLV_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SNOWHICE_dims[0] = time_dim;
    SNOWHICE_dims[1] = lat_dim;
    SNOWHICE_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SNOWHICE", NC_DOUBLE, RANK_SNOWHICE, SNOWHICE_dims, &SNOWHICE_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SNOWHICE_id, NC_CHUNKED, SNOWHICE_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SNOWHLND_dims[0] = time_dim;
    SNOWHLND_dims[1] = lat_dim;
    SNOWHLND_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SNOWHLND", NC_DOUBLE, RANK_SNOWHLND, SNOWHLND_dims, &SNOWHLND_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SNOWHLND_id, NC_CHUNKED, SNOWHLND_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SO2_dims[0] = time_dim;
    SO2_dims[1] = lev_dim;
    SO2_dims[2] = lat_dim;
    SO2_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SO2", NC_DOUBLE, RANK_SO2, SO2_dims, &SO2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SO2_id, NC_CHUNKED, SO2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SO2_CLXF_dims[0] = time_dim;
    SO2_CLXF_dims[1] = lat_dim;
    SO2_CLXF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SO2_CLXF", NC_DOUBLE, RANK_SO2_CLXF, SO2_CLXF_dims, &SO2_CLXF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SO2_CLXF_id, NC_CHUNKED, SO2_CLXF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SO2_XFRC_dims[0] = time_dim;
    SO2_XFRC_dims[1] = lev_dim;
    SO2_XFRC_dims[2] = lat_dim;
    SO2_XFRC_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SO2_XFRC", NC_DOUBLE, RANK_SO2_XFRC, SO2_XFRC_dims, &SO2_XFRC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SO2_XFRC_id, NC_CHUNKED, SO2_XFRC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SOAG_dims[0] = time_dim;
    SOAG_dims[1] = lev_dim;
    SOAG_dims[2] = lat_dim;
    SOAG_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SOAG", NC_DOUBLE, RANK_SOAG, SOAG_dims, &SOAG_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SOAG_id, NC_CHUNKED, SOAG_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SOAG_sfgaex1_dims[0] = time_dim;
    SOAG_sfgaex1_dims[1] = lat_dim;
    SOAG_sfgaex1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SOAG_sfgaex1", NC_DOUBLE, RANK_SOAG_sfgaex1, SOAG_sfgaex1_dims, &SOAG_sfgaex1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SOAG_sfgaex1_id, NC_CHUNKED, SOAG_sfgaex1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SOLIN_dims[0] = time_dim;
    SOLIN_dims[1] = lat_dim;
    SOLIN_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SOLIN", NC_DOUBLE, RANK_SOLIN, SOLIN_dims, &SOLIN_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SOLIN_id, NC_CHUNKED, SOLIN_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SPROD_dims[0] = time_dim;
    SPROD_dims[1] = ilev_dim;
    SPROD_dims[2] = lat_dim;
    SPROD_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SPROD", NC_DOUBLE, RANK_SPROD, SPROD_dims, &SPROD_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SPROD_id, NC_CHUNKED, SPROD_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SRFRAD_dims[0] = time_dim;
    SRFRAD_dims[1] = lat_dim;
    SRFRAD_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SRFRAD", NC_DOUBLE, RANK_SRFRAD, SRFRAD_dims, &SRFRAD_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SRFRAD_id, NC_CHUNKED, SRFRAD_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SSAVIS_dims[0] = time_dim;
    SSAVIS_dims[1] = lat_dim;
    SSAVIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SSAVIS", NC_DOUBLE, RANK_SSAVIS, SSAVIS_dims, &SSAVIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SSAVIS_id, NC_CHUNKED, SSAVIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SSTODXC_dims[0] = time_dim;
    SSTODXC_dims[1] = lat_dim;
    SSTODXC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SSTODXC", NC_DOUBLE, RANK_SSTODXC, SSTODXC_dims, &SSTODXC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SSTODXC_id, NC_CHUNKED, SSTODXC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SSTSFDRY_dims[0] = time_dim;
    SSTSFDRY_dims[1] = lat_dim;
    SSTSFDRY_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SSTSFDRY", NC_DOUBLE, RANK_SSTSFDRY, SSTSFDRY_dims, &SSTSFDRY_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SSTSFDRY_id, NC_CHUNKED, SSTSFDRY_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SSTSFMBL_dims[0] = time_dim;
    SSTSFMBL_dims[1] = lat_dim;
    SSTSFMBL_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SSTSFMBL", NC_DOUBLE, RANK_SSTSFMBL, SSTSFMBL_dims, &SSTSFMBL_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SSTSFMBL_id, NC_CHUNKED, SSTSFMBL_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SSTSFWET_dims[0] = time_dim;
    SSTSFWET_dims[1] = lat_dim;
    SSTSFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SSTSFWET", NC_DOUBLE, RANK_SSTSFWET, SSTSFWET_dims, &SSTSFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SSTSFWET_id, NC_CHUNKED, SSTSFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    SWCF_dims[0] = time_dim;
    SWCF_dims[1] = lat_dim;
    SWCF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "SWCF", NC_DOUBLE, RANK_SWCF, SWCF_dims, &SWCF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SWCF_id, NC_CHUNKED, SWCF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    T_dims[0] = time_dim;
    T_dims[1] = lev_dim;
    T_dims[2] = lat_dim;
    T_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "T", NC_DOUBLE, RANK_T, T_dims, &T_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, T_id, NC_CHUNKED, T_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    TAUTMSX_dims[0] = time_dim;
    TAUTMSX_dims[1] = lat_dim;
    TAUTMSX_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "TAUTMSX", NC_DOUBLE, RANK_TAUTMSX, TAUTMSX_dims, &TAUTMSX_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TAUTMSX_id, NC_CHUNKED, TAUTMSX_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    TAUTMSY_dims[0] = time_dim;
    TAUTMSY_dims[1] = lat_dim;
    TAUTMSY_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "TAUTMSY", NC_DOUBLE, RANK_TAUTMSY, TAUTMSY_dims, &TAUTMSY_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TAUTMSY_id, NC_CHUNKED, TAUTMSY_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    TAUX_dims[0] = time_dim;
    TAUX_dims[1] = lat_dim;
    TAUX_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "TAUX", NC_DOUBLE, RANK_TAUX, TAUX_dims, &TAUX_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TAUX_id, NC_CHUNKED, TAUX_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    TAUY_dims[0] = time_dim;
    TAUY_dims[1] = lat_dim;
    TAUY_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "TAUY", NC_DOUBLE, RANK_TAUY, TAUY_dims, &TAUY_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TAUY_id, NC_CHUNKED, TAUY_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    TGCLDCWP_dims[0] = time_dim;
    TGCLDCWP_dims[1] = lat_dim;
    TGCLDCWP_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "TGCLDCWP", NC_DOUBLE, RANK_TGCLDCWP, TGCLDCWP_dims, &TGCLDCWP_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TGCLDCWP_id, NC_CHUNKED, TGCLDCWP_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    TGCLDIWP_dims[0] = time_dim;
    TGCLDIWP_dims[1] = lat_dim;
    TGCLDIWP_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "TGCLDIWP", NC_DOUBLE, RANK_TGCLDIWP, TGCLDIWP_dims, &TGCLDIWP_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TGCLDIWP_id, NC_CHUNKED, TGCLDIWP_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    TGCLDLWP_dims[0] = time_dim;
    TGCLDLWP_dims[1] = lat_dim;
    TGCLDLWP_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "TGCLDLWP", NC_DOUBLE, RANK_TGCLDLWP, TGCLDLWP_dims, &TGCLDLWP_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TGCLDLWP_id, NC_CHUNKED, TGCLDLWP_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    TKE_dims[0] = time_dim;
    TKE_dims[1] = ilev_dim;
    TKE_dims[2] = lat_dim;
    TKE_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "TKE", NC_DOUBLE, RANK_TKE, TKE_dims, &TKE_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TKE_id, NC_CHUNKED, TKE_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    TMQ_dims[0] = time_dim;
    TMQ_dims[1] = lat_dim;
    TMQ_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "TMQ", NC_DOUBLE, RANK_TMQ, TMQ_dims, &TMQ_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TMQ_id, NC_CHUNKED, TMQ_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    TREFHT_dims[0] = time_dim;
    TREFHT_dims[1] = lat_dim;
    TREFHT_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "TREFHT", NC_DOUBLE, RANK_TREFHT, TREFHT_dims, &TREFHT_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TREFHT_id, NC_CHUNKED, TREFHT_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    TREFMNAV_dims[0] = time_dim;
    TREFMNAV_dims[1] = lat_dim;
    TREFMNAV_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "TREFMNAV", NC_DOUBLE, RANK_TREFMNAV, TREFMNAV_dims, &TREFMNAV_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TREFMNAV_id, NC_CHUNKED, TREFMNAV_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    TREFMXAV_dims[0] = time_dim;
    TREFMXAV_dims[1] = lat_dim;
    TREFMXAV_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "TREFMXAV", NC_DOUBLE, RANK_TREFMXAV, TREFMXAV_dims, &TREFMXAV_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TREFMXAV_id, NC_CHUNKED, TREFMXAV_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    TROP_FD_dims[0] = time_dim;
    TROP_FD_dims[1] = lat_dim;
    TROP_FD_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "TROP_FD", NC_DOUBLE, RANK_TROP_FD, TROP_FD_dims, &TROP_FD_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_FD_id, NC_CHUNKED, TROP_FD_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    TROP_P_dims[0] = time_dim;
    TROP_P_dims[1] = lat_dim;
    TROP_P_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "TROP_P", NC_DOUBLE, RANK_TROP_P, TROP_P_dims, &TROP_P_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_P_id, NC_CHUNKED, TROP_P_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    TROP_PD_dims[0] = time_dim;
    TROP_PD_dims[1] = lev_dim;
    TROP_PD_dims[2] = lat_dim;
    TROP_PD_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "TROP_PD", NC_DOUBLE, RANK_TROP_PD, TROP_PD_dims, &TROP_PD_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_PD_id, NC_CHUNKED, TROP_PD_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    TROP_T_dims[0] = time_dim;
    TROP_T_dims[1] = lat_dim;
    TROP_T_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "TROP_T", NC_DOUBLE, RANK_TROP_T, TROP_T_dims, &TROP_T_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_T_id, NC_CHUNKED, TROP_T_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    TROP_Z_dims[0] = time_dim;
    TROP_Z_dims[1] = lat_dim;
    TROP_Z_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "TROP_Z", NC_DOUBLE, RANK_TROP_Z, TROP_Z_dims, &TROP_Z_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_Z_id, NC_CHUNKED, TROP_Z_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    TS_dims[0] = time_dim;
    TS_dims[1] = lat_dim;
    TS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "TS", NC_DOUBLE, RANK_TS, TS_dims, &TS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TS_id, NC_CHUNKED, TS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    TSMN_dims[0] = time_dim;
    TSMN_dims[1] = lat_dim;
    TSMN_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "TSMN", NC_DOUBLE, RANK_TSMN, TSMN_dims, &TSMN_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TSMN_id, NC_CHUNKED, TSMN_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    TSMX_dims[0] = time_dim;
    TSMX_dims[1] = lat_dim;
    TSMX_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "TSMX", NC_DOUBLE, RANK_TSMX, TSMX_dims, &TSMX_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TSMX_id, NC_CHUNKED, TSMX_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    U_dims[0] = time_dim;
    U_dims[1] = lev_dim;
    U_dims[2] = lat_dim;
    U_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "U", NC_DOUBLE, RANK_U, U_dims, &U_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, U_id, NC_CHUNKED, U_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    UFLX_dims[0] = time_dim;
    UFLX_dims[1] = ilev_dim;
    UFLX_dims[2] = lat_dim;
    UFLX_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "UFLX", NC_DOUBLE, RANK_UFLX, UFLX_dims, &UFLX_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, UFLX_id, NC_CHUNKED, UFLX_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    US_dims[0] = time_dim;
    US_dims[1] = lev_dim;
    US_dims[2] = slat_dim;
    US_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "US", NC_DOUBLE, RANK_US, US_dims, &US_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, US_id, NC_CHUNKED, US_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    UU_dims[0] = time_dim;
    UU_dims[1] = lev_dim;
    UU_dims[2] = lat_dim;
    UU_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "UU", NC_DOUBLE, RANK_UU, UU_dims, &UU_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, UU_id, NC_CHUNKED, UU_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    V_dims[0] = time_dim;
    V_dims[1] = lev_dim;
    V_dims[2] = lat_dim;
    V_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "V", NC_DOUBLE, RANK_V, V_dims, &V_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, V_id, NC_CHUNKED, V_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    VD01_dims[0] = time_dim;
    VD01_dims[1] = lev_dim;
    VD01_dims[2] = lat_dim;
    VD01_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "VD01", NC_DOUBLE, RANK_VD01, VD01_dims, &VD01_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VD01_id, NC_CHUNKED, VD01_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    VFLX_dims[0] = time_dim;
    VFLX_dims[1] = ilev_dim;
    VFLX_dims[2] = lat_dim;
    VFLX_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "VFLX", NC_DOUBLE, RANK_VFLX, VFLX_dims, &VFLX_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VFLX_id, NC_CHUNKED, VFLX_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    VQ_dims[0] = time_dim;
    VQ_dims[1] = lev_dim;
    VQ_dims[2] = lat_dim;
    VQ_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "VQ", NC_DOUBLE, RANK_VQ, VQ_dims, &VQ_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VQ_id, NC_CHUNKED, VQ_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    VS_dims[0] = time_dim;
    VS_dims[1] = lev_dim;
    VS_dims[2] = lat_dim;
    VS_dims[3] = slon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "VS", NC_DOUBLE, RANK_VS, VS_dims, &VS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VS_id, NC_CHUNKED, VS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    VT_dims[0] = time_dim;
    VT_dims[1] = lev_dim;
    VT_dims[2] = lat_dim;
    VT_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "VT", NC_DOUBLE, RANK_VT, VT_dims, &VT_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VT_id, NC_CHUNKED, VT_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    VU_dims[0] = time_dim;
    VU_dims[1] = lev_dim;
    VU_dims[2] = lat_dim;
    VU_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "VU", NC_DOUBLE, RANK_VU, VU_dims, &VU_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VU_id, NC_CHUNKED, VU_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    VV_dims[0] = time_dim;
    VV_dims[1] = lev_dim;
    VV_dims[2] = lat_dim;
    VV_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "VV", NC_DOUBLE, RANK_VV, VV_dims, &VV_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VV_id, NC_CHUNKED, VV_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    WGUSTD_dims[0] = time_dim;
    WGUSTD_dims[1] = lat_dim;
    WGUSTD_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "WGUSTD", NC_DOUBLE, RANK_WGUSTD, WGUSTD_dims, &WGUSTD_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, WGUSTD_id, NC_CHUNKED, WGUSTD_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    WTKE_dims[0] = time_dim;
    WTKE_dims[1] = lev_dim;
    WTKE_dims[2] = lat_dim;
    WTKE_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "WTKE", NC_DOUBLE, RANK_WTKE, WTKE_dims, &WTKE_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, WTKE_id, NC_CHUNKED, WTKE_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    XPH_LWC_dims[0] = time_dim;
    XPH_LWC_dims[1] = lev_dim;
    XPH_LWC_dims[2] = lat_dim;
    XPH_LWC_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "XPH_LWC", NC_DOUBLE, RANK_XPH_LWC, XPH_LWC_dims, &XPH_LWC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, XPH_LWC_id, NC_CHUNKED, XPH_LWC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    Z3_dims[0] = time_dim;
    Z3_dims[1] = lev_dim;
    Z3_dims[2] = lat_dim;
    Z3_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "Z3", NC_DOUBLE, RANK_Z3, Z3_dims, &Z3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, Z3_id, NC_CHUNKED, Z3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    airFV_dims[0] = time_dim;
    airFV_dims[1] = lat_dim;
    airFV_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "airFV", NC_DOUBLE, RANK_airFV, airFV_dims, &airFV_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, airFV_id, NC_CHUNKED, airFV_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    bc_a1_dims[0] = time_dim;
    bc_a1_dims[1] = lev_dim;
    bc_a1_dims[2] = lat_dim;
    bc_a1_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "bc_a1", NC_DOUBLE, RANK_bc_a1, bc_a1_dims, &bc_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1_id, NC_CHUNKED, bc_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    bc_a1DDF_dims[0] = time_dim;
    bc_a1DDF_dims[1] = lat_dim;
    bc_a1DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "bc_a1DDF", NC_DOUBLE, RANK_bc_a1DDF, bc_a1DDF_dims, &bc_a1DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1DDF_id, NC_CHUNKED, bc_a1DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    bc_a1GVF_dims[0] = time_dim;
    bc_a1GVF_dims[1] = lat_dim;
    bc_a1GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "bc_a1GVF", NC_DOUBLE, RANK_bc_a1GVF, bc_a1GVF_dims, &bc_a1GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1GVF_id, NC_CHUNKED, bc_a1GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    bc_a1SFSBC_dims[0] = time_dim;
    bc_a1SFSBC_dims[1] = lat_dim;
    bc_a1SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "bc_a1SFSBC", NC_DOUBLE, RANK_bc_a1SFSBC, bc_a1SFSBC_dims, &bc_a1SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1SFSBC_id, NC_CHUNKED, bc_a1SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    bc_a1SFSBS_dims[0] = time_dim;
    bc_a1SFSBS_dims[1] = lat_dim;
    bc_a1SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "bc_a1SFSBS", NC_DOUBLE, RANK_bc_a1SFSBS, bc_a1SFSBS_dims, &bc_a1SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1SFSBS_id, NC_CHUNKED, bc_a1SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    bc_a1SFSIC_dims[0] = time_dim;
    bc_a1SFSIC_dims[1] = lat_dim;
    bc_a1SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "bc_a1SFSIC", NC_DOUBLE, RANK_bc_a1SFSIC, bc_a1SFSIC_dims, &bc_a1SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1SFSIC_id, NC_CHUNKED, bc_a1SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    bc_a1SFSIS_dims[0] = time_dim;
    bc_a1SFSIS_dims[1] = lat_dim;
    bc_a1SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "bc_a1SFSIS", NC_DOUBLE, RANK_bc_a1SFSIS, bc_a1SFSIS_dims, &bc_a1SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1SFSIS_id, NC_CHUNKED, bc_a1SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    bc_a1SFWET_dims[0] = time_dim;
    bc_a1SFWET_dims[1] = lat_dim;
    bc_a1SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "bc_a1SFWET", NC_DOUBLE, RANK_bc_a1SFWET, bc_a1SFWET_dims, &bc_a1SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1SFWET_id, NC_CHUNKED, bc_a1SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    bc_a1TBF_dims[0] = time_dim;
    bc_a1TBF_dims[1] = lat_dim;
    bc_a1TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "bc_a1TBF", NC_DOUBLE, RANK_bc_a1TBF, bc_a1TBF_dims, &bc_a1TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1TBF_id, NC_CHUNKED, bc_a1TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    bc_a1_CLXF_dims[0] = time_dim;
    bc_a1_CLXF_dims[1] = lat_dim;
    bc_a1_CLXF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "bc_a1_CLXF", NC_DOUBLE, RANK_bc_a1_CLXF, bc_a1_CLXF_dims, &bc_a1_CLXF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1_CLXF_id, NC_CHUNKED, bc_a1_CLXF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    bc_a1_XFRC_dims[0] = time_dim;
    bc_a1_XFRC_dims[1] = lev_dim;
    bc_a1_XFRC_dims[2] = lat_dim;
    bc_a1_XFRC_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "bc_a1_XFRC", NC_DOUBLE, RANK_bc_a1_XFRC, bc_a1_XFRC_dims, &bc_a1_XFRC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1_XFRC_id, NC_CHUNKED, bc_a1_XFRC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    bc_c1_dims[0] = time_dim;
    bc_c1_dims[1] = lev_dim;
    bc_c1_dims[2] = lat_dim;
    bc_c1_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "bc_c1", NC_DOUBLE, RANK_bc_c1, bc_c1_dims, &bc_c1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1_id, NC_CHUNKED, bc_c1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    bc_c1DDF_dims[0] = time_dim;
    bc_c1DDF_dims[1] = lat_dim;
    bc_c1DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "bc_c1DDF", NC_DOUBLE, RANK_bc_c1DDF, bc_c1DDF_dims, &bc_c1DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1DDF_id, NC_CHUNKED, bc_c1DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    bc_c1GVF_dims[0] = time_dim;
    bc_c1GVF_dims[1] = lat_dim;
    bc_c1GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "bc_c1GVF", NC_DOUBLE, RANK_bc_c1GVF, bc_c1GVF_dims, &bc_c1GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1GVF_id, NC_CHUNKED, bc_c1GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    bc_c1SFSBC_dims[0] = time_dim;
    bc_c1SFSBC_dims[1] = lat_dim;
    bc_c1SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "bc_c1SFSBC", NC_DOUBLE, RANK_bc_c1SFSBC, bc_c1SFSBC_dims, &bc_c1SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1SFSBC_id, NC_CHUNKED, bc_c1SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    bc_c1SFSBS_dims[0] = time_dim;
    bc_c1SFSBS_dims[1] = lat_dim;
    bc_c1SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "bc_c1SFSBS", NC_DOUBLE, RANK_bc_c1SFSBS, bc_c1SFSBS_dims, &bc_c1SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1SFSBS_id, NC_CHUNKED, bc_c1SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    bc_c1SFSIC_dims[0] = time_dim;
    bc_c1SFSIC_dims[1] = lat_dim;
    bc_c1SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "bc_c1SFSIC", NC_DOUBLE, RANK_bc_c1SFSIC, bc_c1SFSIC_dims, &bc_c1SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1SFSIC_id, NC_CHUNKED, bc_c1SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    bc_c1SFSIS_dims[0] = time_dim;
    bc_c1SFSIS_dims[1] = lat_dim;
    bc_c1SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "bc_c1SFSIS", NC_DOUBLE, RANK_bc_c1SFSIS, bc_c1SFSIS_dims, &bc_c1SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1SFSIS_id, NC_CHUNKED, bc_c1SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    bc_c1SFWET_dims[0] = time_dim;
    bc_c1SFWET_dims[1] = lat_dim;
    bc_c1SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "bc_c1SFWET", NC_DOUBLE, RANK_bc_c1SFWET, bc_c1SFWET_dims, &bc_c1SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1SFWET_id, NC_CHUNKED, bc_c1SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    bc_c1TBF_dims[0] = time_dim;
    bc_c1TBF_dims[1] = lat_dim;
    bc_c1TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "bc_c1TBF", NC_DOUBLE, RANK_bc_c1TBF, bc_c1TBF_dims, &bc_c1TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1TBF_id, NC_CHUNKED, bc_c1TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    chem_trop_dims[0] = time_dim;
    chem_trop_dims[1] = lev_dim;
    chem_trop_dims[2] = lat_dim;
    chem_trop_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "chem_trop", NC_DOUBLE, RANK_chem_trop, chem_trop_dims, &chem_trop_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, chem_trop_id, NC_CHUNKED, chem_trop_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    chem_trop_tropop_dims[0] = time_dim;
    chem_trop_tropop_dims[1] = lev_dim;
    chem_trop_tropop_dims[2] = lat_dim;
    chem_trop_tropop_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "chem_trop_tropop", NC_DOUBLE, RANK_chem_trop_tropop, chem_trop_tropop_dims, &chem_trop_tropop_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, chem_trop_tropop_id, NC_CHUNKED, chem_trop_tropop_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dgnd_a01_dims[0] = time_dim;
    dgnd_a01_dims[1] = lev_dim;
    dgnd_a01_dims[2] = lat_dim;
    dgnd_a01_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dgnd_a01", NC_DOUBLE, RANK_dgnd_a01, dgnd_a01_dims, &dgnd_a01_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnd_a01_id, NC_CHUNKED, dgnd_a01_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dgnd_a02_dims[0] = time_dim;
    dgnd_a02_dims[1] = lev_dim;
    dgnd_a02_dims[2] = lat_dim;
    dgnd_a02_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dgnd_a02", NC_DOUBLE, RANK_dgnd_a02, dgnd_a02_dims, &dgnd_a02_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnd_a02_id, NC_CHUNKED, dgnd_a02_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dgnd_a03_dims[0] = time_dim;
    dgnd_a03_dims[1] = lev_dim;
    dgnd_a03_dims[2] = lat_dim;
    dgnd_a03_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dgnd_a03", NC_DOUBLE, RANK_dgnd_a03, dgnd_a03_dims, &dgnd_a03_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnd_a03_id, NC_CHUNKED, dgnd_a03_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dgnw_a01_dims[0] = time_dim;
    dgnw_a01_dims[1] = lev_dim;
    dgnw_a01_dims[2] = lat_dim;
    dgnw_a01_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dgnw_a01", NC_DOUBLE, RANK_dgnw_a01, dgnw_a01_dims, &dgnw_a01_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnw_a01_id, NC_CHUNKED, dgnw_a01_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dgnw_a02_dims[0] = time_dim;
    dgnw_a02_dims[1] = lev_dim;
    dgnw_a02_dims[2] = lat_dim;
    dgnw_a02_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dgnw_a02", NC_DOUBLE, RANK_dgnw_a02, dgnw_a02_dims, &dgnw_a02_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnw_a02_id, NC_CHUNKED, dgnw_a02_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dgnw_a03_dims[0] = time_dim;
    dgnw_a03_dims[1] = lev_dim;
    dgnw_a03_dims[2] = lat_dim;
    dgnw_a03_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dgnw_a03", NC_DOUBLE, RANK_dgnw_a03, dgnw_a03_dims, &dgnw_a03_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnw_a03_id, NC_CHUNKED, dgnw_a03_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_a1_dims[0] = time_dim;
    dst_a1_dims[1] = lev_dim;
    dst_a1_dims[2] = lat_dim;
    dst_a1_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_a1", NC_DOUBLE, RANK_dst_a1, dst_a1_dims, &dst_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1_id, NC_CHUNKED, dst_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_a1DDF_dims[0] = time_dim;
    dst_a1DDF_dims[1] = lat_dim;
    dst_a1DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_a1DDF", NC_DOUBLE, RANK_dst_a1DDF, dst_a1DDF_dims, &dst_a1DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1DDF_id, NC_CHUNKED, dst_a1DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_a1GVF_dims[0] = time_dim;
    dst_a1GVF_dims[1] = lat_dim;
    dst_a1GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_a1GVF", NC_DOUBLE, RANK_dst_a1GVF, dst_a1GVF_dims, &dst_a1GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1GVF_id, NC_CHUNKED, dst_a1GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_a1SF_dims[0] = time_dim;
    dst_a1SF_dims[1] = lat_dim;
    dst_a1SF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_a1SF", NC_DOUBLE, RANK_dst_a1SF, dst_a1SF_dims, &dst_a1SF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SF_id, NC_CHUNKED, dst_a1SF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_a1SFSBC_dims[0] = time_dim;
    dst_a1SFSBC_dims[1] = lat_dim;
    dst_a1SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_a1SFSBC", NC_DOUBLE, RANK_dst_a1SFSBC, dst_a1SFSBC_dims, &dst_a1SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SFSBC_id, NC_CHUNKED, dst_a1SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_a1SFSBS_dims[0] = time_dim;
    dst_a1SFSBS_dims[1] = lat_dim;
    dst_a1SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_a1SFSBS", NC_DOUBLE, RANK_dst_a1SFSBS, dst_a1SFSBS_dims, &dst_a1SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SFSBS_id, NC_CHUNKED, dst_a1SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_a1SFSIC_dims[0] = time_dim;
    dst_a1SFSIC_dims[1] = lat_dim;
    dst_a1SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_a1SFSIC", NC_DOUBLE, RANK_dst_a1SFSIC, dst_a1SFSIC_dims, &dst_a1SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SFSIC_id, NC_CHUNKED, dst_a1SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_a1SFSIS_dims[0] = time_dim;
    dst_a1SFSIS_dims[1] = lat_dim;
    dst_a1SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_a1SFSIS", NC_DOUBLE, RANK_dst_a1SFSIS, dst_a1SFSIS_dims, &dst_a1SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SFSIS_id, NC_CHUNKED, dst_a1SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_a1SFWET_dims[0] = time_dim;
    dst_a1SFWET_dims[1] = lat_dim;
    dst_a1SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_a1SFWET", NC_DOUBLE, RANK_dst_a1SFWET, dst_a1SFWET_dims, &dst_a1SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SFWET_id, NC_CHUNKED, dst_a1SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_a1TBF_dims[0] = time_dim;
    dst_a1TBF_dims[1] = lat_dim;
    dst_a1TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_a1TBF", NC_DOUBLE, RANK_dst_a1TBF, dst_a1TBF_dims, &dst_a1TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1TBF_id, NC_CHUNKED, dst_a1TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_a3_dims[0] = time_dim;
    dst_a3_dims[1] = lev_dim;
    dst_a3_dims[2] = lat_dim;
    dst_a3_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_a3", NC_DOUBLE, RANK_dst_a3, dst_a3_dims, &dst_a3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3_id, NC_CHUNKED, dst_a3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_a3DDF_dims[0] = time_dim;
    dst_a3DDF_dims[1] = lat_dim;
    dst_a3DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_a3DDF", NC_DOUBLE, RANK_dst_a3DDF, dst_a3DDF_dims, &dst_a3DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3DDF_id, NC_CHUNKED, dst_a3DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_a3GVF_dims[0] = time_dim;
    dst_a3GVF_dims[1] = lat_dim;
    dst_a3GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_a3GVF", NC_DOUBLE, RANK_dst_a3GVF, dst_a3GVF_dims, &dst_a3GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3GVF_id, NC_CHUNKED, dst_a3GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_a3SF_dims[0] = time_dim;
    dst_a3SF_dims[1] = lat_dim;
    dst_a3SF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_a3SF", NC_DOUBLE, RANK_dst_a3SF, dst_a3SF_dims, &dst_a3SF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SF_id, NC_CHUNKED, dst_a3SF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_a3SFSBC_dims[0] = time_dim;
    dst_a3SFSBC_dims[1] = lat_dim;
    dst_a3SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_a3SFSBC", NC_DOUBLE, RANK_dst_a3SFSBC, dst_a3SFSBC_dims, &dst_a3SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SFSBC_id, NC_CHUNKED, dst_a3SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_a3SFSBS_dims[0] = time_dim;
    dst_a3SFSBS_dims[1] = lat_dim;
    dst_a3SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_a3SFSBS", NC_DOUBLE, RANK_dst_a3SFSBS, dst_a3SFSBS_dims, &dst_a3SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SFSBS_id, NC_CHUNKED, dst_a3SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_a3SFSIC_dims[0] = time_dim;
    dst_a3SFSIC_dims[1] = lat_dim;
    dst_a3SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_a3SFSIC", NC_DOUBLE, RANK_dst_a3SFSIC, dst_a3SFSIC_dims, &dst_a3SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SFSIC_id, NC_CHUNKED, dst_a3SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_a3SFSIS_dims[0] = time_dim;
    dst_a3SFSIS_dims[1] = lat_dim;
    dst_a3SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_a3SFSIS", NC_DOUBLE, RANK_dst_a3SFSIS, dst_a3SFSIS_dims, &dst_a3SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SFSIS_id, NC_CHUNKED, dst_a3SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_a3SFWET_dims[0] = time_dim;
    dst_a3SFWET_dims[1] = lat_dim;
    dst_a3SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_a3SFWET", NC_DOUBLE, RANK_dst_a3SFWET, dst_a3SFWET_dims, &dst_a3SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SFWET_id, NC_CHUNKED, dst_a3SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_a3TBF_dims[0] = time_dim;
    dst_a3TBF_dims[1] = lat_dim;
    dst_a3TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_a3TBF", NC_DOUBLE, RANK_dst_a3TBF, dst_a3TBF_dims, &dst_a3TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3TBF_id, NC_CHUNKED, dst_a3TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_c1_dims[0] = time_dim;
    dst_c1_dims[1] = lev_dim;
    dst_c1_dims[2] = lat_dim;
    dst_c1_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_c1", NC_DOUBLE, RANK_dst_c1, dst_c1_dims, &dst_c1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1_id, NC_CHUNKED, dst_c1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_c1DDF_dims[0] = time_dim;
    dst_c1DDF_dims[1] = lat_dim;
    dst_c1DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_c1DDF", NC_DOUBLE, RANK_dst_c1DDF, dst_c1DDF_dims, &dst_c1DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1DDF_id, NC_CHUNKED, dst_c1DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_c1GVF_dims[0] = time_dim;
    dst_c1GVF_dims[1] = lat_dim;
    dst_c1GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_c1GVF", NC_DOUBLE, RANK_dst_c1GVF, dst_c1GVF_dims, &dst_c1GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1GVF_id, NC_CHUNKED, dst_c1GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_c1SFSBC_dims[0] = time_dim;
    dst_c1SFSBC_dims[1] = lat_dim;
    dst_c1SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_c1SFSBC", NC_DOUBLE, RANK_dst_c1SFSBC, dst_c1SFSBC_dims, &dst_c1SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1SFSBC_id, NC_CHUNKED, dst_c1SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_c1SFSBS_dims[0] = time_dim;
    dst_c1SFSBS_dims[1] = lat_dim;
    dst_c1SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_c1SFSBS", NC_DOUBLE, RANK_dst_c1SFSBS, dst_c1SFSBS_dims, &dst_c1SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1SFSBS_id, NC_CHUNKED, dst_c1SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_c1SFSIC_dims[0] = time_dim;
    dst_c1SFSIC_dims[1] = lat_dim;
    dst_c1SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_c1SFSIC", NC_DOUBLE, RANK_dst_c1SFSIC, dst_c1SFSIC_dims, &dst_c1SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1SFSIC_id, NC_CHUNKED, dst_c1SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_c1SFSIS_dims[0] = time_dim;
    dst_c1SFSIS_dims[1] = lat_dim;
    dst_c1SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_c1SFSIS", NC_DOUBLE, RANK_dst_c1SFSIS, dst_c1SFSIS_dims, &dst_c1SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1SFSIS_id, NC_CHUNKED, dst_c1SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_c1SFWET_dims[0] = time_dim;
    dst_c1SFWET_dims[1] = lat_dim;
    dst_c1SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_c1SFWET", NC_DOUBLE, RANK_dst_c1SFWET, dst_c1SFWET_dims, &dst_c1SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1SFWET_id, NC_CHUNKED, dst_c1SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_c1TBF_dims[0] = time_dim;
    dst_c1TBF_dims[1] = lat_dim;
    dst_c1TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_c1TBF", NC_DOUBLE, RANK_dst_c1TBF, dst_c1TBF_dims, &dst_c1TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1TBF_id, NC_CHUNKED, dst_c1TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_c3_dims[0] = time_dim;
    dst_c3_dims[1] = lev_dim;
    dst_c3_dims[2] = lat_dim;
    dst_c3_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_c3", NC_DOUBLE, RANK_dst_c3, dst_c3_dims, &dst_c3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3_id, NC_CHUNKED, dst_c3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_c3DDF_dims[0] = time_dim;
    dst_c3DDF_dims[1] = lat_dim;
    dst_c3DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_c3DDF", NC_DOUBLE, RANK_dst_c3DDF, dst_c3DDF_dims, &dst_c3DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3DDF_id, NC_CHUNKED, dst_c3DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_c3GVF_dims[0] = time_dim;
    dst_c3GVF_dims[1] = lat_dim;
    dst_c3GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_c3GVF", NC_DOUBLE, RANK_dst_c3GVF, dst_c3GVF_dims, &dst_c3GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3GVF_id, NC_CHUNKED, dst_c3GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_c3SFSBC_dims[0] = time_dim;
    dst_c3SFSBC_dims[1] = lat_dim;
    dst_c3SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_c3SFSBC", NC_DOUBLE, RANK_dst_c3SFSBC, dst_c3SFSBC_dims, &dst_c3SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3SFSBC_id, NC_CHUNKED, dst_c3SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_c3SFSBS_dims[0] = time_dim;
    dst_c3SFSBS_dims[1] = lat_dim;
    dst_c3SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_c3SFSBS", NC_DOUBLE, RANK_dst_c3SFSBS, dst_c3SFSBS_dims, &dst_c3SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3SFSBS_id, NC_CHUNKED, dst_c3SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_c3SFSIC_dims[0] = time_dim;
    dst_c3SFSIC_dims[1] = lat_dim;
    dst_c3SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_c3SFSIC", NC_DOUBLE, RANK_dst_c3SFSIC, dst_c3SFSIC_dims, &dst_c3SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3SFSIC_id, NC_CHUNKED, dst_c3SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_c3SFSIS_dims[0] = time_dim;
    dst_c3SFSIS_dims[1] = lat_dim;
    dst_c3SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_c3SFSIS", NC_DOUBLE, RANK_dst_c3SFSIS, dst_c3SFSIS_dims, &dst_c3SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3SFSIS_id, NC_CHUNKED, dst_c3SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_c3SFWET_dims[0] = time_dim;
    dst_c3SFWET_dims[1] = lat_dim;
    dst_c3SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_c3SFWET", NC_DOUBLE, RANK_dst_c3SFWET, dst_c3SFWET_dims, &dst_c3SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3SFWET_id, NC_CHUNKED, dst_c3SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    dst_c3TBF_dims[0] = time_dim;
    dst_c3TBF_dims[1] = lat_dim;
    dst_c3TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "dst_c3TBF", NC_DOUBLE, RANK_dst_c3TBF, dst_c3TBF_dims, &dst_c3TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3TBF_id, NC_CHUNKED, dst_c3TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a1_dims[0] = time_dim;
    ncl_a1_dims[1] = lev_dim;
    ncl_a1_dims[2] = lat_dim;
    ncl_a1_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a1", NC_DOUBLE, RANK_ncl_a1, ncl_a1_dims, &ncl_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1_id, NC_CHUNKED, ncl_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a1DDF_dims[0] = time_dim;
    ncl_a1DDF_dims[1] = lat_dim;
    ncl_a1DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a1DDF", NC_DOUBLE, RANK_ncl_a1DDF, ncl_a1DDF_dims, &ncl_a1DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1DDF_id, NC_CHUNKED, ncl_a1DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a1GVF_dims[0] = time_dim;
    ncl_a1GVF_dims[1] = lat_dim;
    ncl_a1GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a1GVF", NC_DOUBLE, RANK_ncl_a1GVF, ncl_a1GVF_dims, &ncl_a1GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1GVF_id, NC_CHUNKED, ncl_a1GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a1SF_dims[0] = time_dim;
    ncl_a1SF_dims[1] = lat_dim;
    ncl_a1SF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a1SF", NC_DOUBLE, RANK_ncl_a1SF, ncl_a1SF_dims, &ncl_a1SF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SF_id, NC_CHUNKED, ncl_a1SF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a1SFSBC_dims[0] = time_dim;
    ncl_a1SFSBC_dims[1] = lat_dim;
    ncl_a1SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a1SFSBC", NC_DOUBLE, RANK_ncl_a1SFSBC, ncl_a1SFSBC_dims, &ncl_a1SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SFSBC_id, NC_CHUNKED, ncl_a1SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a1SFSBS_dims[0] = time_dim;
    ncl_a1SFSBS_dims[1] = lat_dim;
    ncl_a1SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a1SFSBS", NC_DOUBLE, RANK_ncl_a1SFSBS, ncl_a1SFSBS_dims, &ncl_a1SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SFSBS_id, NC_CHUNKED, ncl_a1SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a1SFSIC_dims[0] = time_dim;
    ncl_a1SFSIC_dims[1] = lat_dim;
    ncl_a1SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a1SFSIC", NC_DOUBLE, RANK_ncl_a1SFSIC, ncl_a1SFSIC_dims, &ncl_a1SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SFSIC_id, NC_CHUNKED, ncl_a1SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a1SFSIS_dims[0] = time_dim;
    ncl_a1SFSIS_dims[1] = lat_dim;
    ncl_a1SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a1SFSIS", NC_DOUBLE, RANK_ncl_a1SFSIS, ncl_a1SFSIS_dims, &ncl_a1SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SFSIS_id, NC_CHUNKED, ncl_a1SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a1SFWET_dims[0] = time_dim;
    ncl_a1SFWET_dims[1] = lat_dim;
    ncl_a1SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a1SFWET", NC_DOUBLE, RANK_ncl_a1SFWET, ncl_a1SFWET_dims, &ncl_a1SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SFWET_id, NC_CHUNKED, ncl_a1SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a1TBF_dims[0] = time_dim;
    ncl_a1TBF_dims[1] = lat_dim;
    ncl_a1TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a1TBF", NC_DOUBLE, RANK_ncl_a1TBF, ncl_a1TBF_dims, &ncl_a1TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1TBF_id, NC_CHUNKED, ncl_a1TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a1_sfcoag1_dims[0] = time_dim;
    ncl_a1_sfcoag1_dims[1] = lat_dim;
    ncl_a1_sfcoag1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a1_sfcoag1", NC_DOUBLE, RANK_ncl_a1_sfcoag1, ncl_a1_sfcoag1_dims, &ncl_a1_sfcoag1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1_sfcoag1_id, NC_CHUNKED, ncl_a1_sfcoag1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a1_sfcsiz3_dims[0] = time_dim;
    ncl_a1_sfcsiz3_dims[1] = lat_dim;
    ncl_a1_sfcsiz3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a1_sfcsiz3", NC_DOUBLE, RANK_ncl_a1_sfcsiz3, ncl_a1_sfcsiz3_dims, &ncl_a1_sfcsiz3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1_sfcsiz3_id, NC_CHUNKED, ncl_a1_sfcsiz3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a1_sfcsiz4_dims[0] = time_dim;
    ncl_a1_sfcsiz4_dims[1] = lat_dim;
    ncl_a1_sfcsiz4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a1_sfcsiz4", NC_DOUBLE, RANK_ncl_a1_sfcsiz4, ncl_a1_sfcsiz4_dims, &ncl_a1_sfcsiz4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1_sfcsiz4_id, NC_CHUNKED, ncl_a1_sfcsiz4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a1_sfgaex2_dims[0] = time_dim;
    ncl_a1_sfgaex2_dims[1] = lat_dim;
    ncl_a1_sfgaex2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a1_sfgaex2", NC_DOUBLE, RANK_ncl_a1_sfgaex2, ncl_a1_sfgaex2_dims, &ncl_a1_sfgaex2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1_sfgaex2_id, NC_CHUNKED, ncl_a1_sfgaex2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a2_dims[0] = time_dim;
    ncl_a2_dims[1] = lev_dim;
    ncl_a2_dims[2] = lat_dim;
    ncl_a2_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a2", NC_DOUBLE, RANK_ncl_a2, ncl_a2_dims, &ncl_a2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2_id, NC_CHUNKED, ncl_a2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a2DDF_dims[0] = time_dim;
    ncl_a2DDF_dims[1] = lat_dim;
    ncl_a2DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a2DDF", NC_DOUBLE, RANK_ncl_a2DDF, ncl_a2DDF_dims, &ncl_a2DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2DDF_id, NC_CHUNKED, ncl_a2DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a2GVF_dims[0] = time_dim;
    ncl_a2GVF_dims[1] = lat_dim;
    ncl_a2GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a2GVF", NC_DOUBLE, RANK_ncl_a2GVF, ncl_a2GVF_dims, &ncl_a2GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2GVF_id, NC_CHUNKED, ncl_a2GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a2SF_dims[0] = time_dim;
    ncl_a2SF_dims[1] = lat_dim;
    ncl_a2SF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a2SF", NC_DOUBLE, RANK_ncl_a2SF, ncl_a2SF_dims, &ncl_a2SF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SF_id, NC_CHUNKED, ncl_a2SF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a2SFSBC_dims[0] = time_dim;
    ncl_a2SFSBC_dims[1] = lat_dim;
    ncl_a2SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a2SFSBC", NC_DOUBLE, RANK_ncl_a2SFSBC, ncl_a2SFSBC_dims, &ncl_a2SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SFSBC_id, NC_CHUNKED, ncl_a2SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a2SFSBS_dims[0] = time_dim;
    ncl_a2SFSBS_dims[1] = lat_dim;
    ncl_a2SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a2SFSBS", NC_DOUBLE, RANK_ncl_a2SFSBS, ncl_a2SFSBS_dims, &ncl_a2SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SFSBS_id, NC_CHUNKED, ncl_a2SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a2SFSIC_dims[0] = time_dim;
    ncl_a2SFSIC_dims[1] = lat_dim;
    ncl_a2SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a2SFSIC", NC_DOUBLE, RANK_ncl_a2SFSIC, ncl_a2SFSIC_dims, &ncl_a2SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SFSIC_id, NC_CHUNKED, ncl_a2SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a2SFSIS_dims[0] = time_dim;
    ncl_a2SFSIS_dims[1] = lat_dim;
    ncl_a2SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a2SFSIS", NC_DOUBLE, RANK_ncl_a2SFSIS, ncl_a2SFSIS_dims, &ncl_a2SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SFSIS_id, NC_CHUNKED, ncl_a2SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a2SFWET_dims[0] = time_dim;
    ncl_a2SFWET_dims[1] = lat_dim;
    ncl_a2SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a2SFWET", NC_DOUBLE, RANK_ncl_a2SFWET, ncl_a2SFWET_dims, &ncl_a2SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SFWET_id, NC_CHUNKED, ncl_a2SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a2TBF_dims[0] = time_dim;
    ncl_a2TBF_dims[1] = lat_dim;
    ncl_a2TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a2TBF", NC_DOUBLE, RANK_ncl_a2TBF, ncl_a2TBF_dims, &ncl_a2TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2TBF_id, NC_CHUNKED, ncl_a2TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a2_sfcoag1_dims[0] = time_dim;
    ncl_a2_sfcoag1_dims[1] = lat_dim;
    ncl_a2_sfcoag1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a2_sfcoag1", NC_DOUBLE, RANK_ncl_a2_sfcoag1, ncl_a2_sfcoag1_dims, &ncl_a2_sfcoag1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2_sfcoag1_id, NC_CHUNKED, ncl_a2_sfcoag1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a2_sfcsiz3_dims[0] = time_dim;
    ncl_a2_sfcsiz3_dims[1] = lat_dim;
    ncl_a2_sfcsiz3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a2_sfcsiz3", NC_DOUBLE, RANK_ncl_a2_sfcsiz3, ncl_a2_sfcsiz3_dims, &ncl_a2_sfcsiz3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2_sfcsiz3_id, NC_CHUNKED, ncl_a2_sfcsiz3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a2_sfcsiz4_dims[0] = time_dim;
    ncl_a2_sfcsiz4_dims[1] = lat_dim;
    ncl_a2_sfcsiz4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a2_sfcsiz4", NC_DOUBLE, RANK_ncl_a2_sfcsiz4, ncl_a2_sfcsiz4_dims, &ncl_a2_sfcsiz4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2_sfcsiz4_id, NC_CHUNKED, ncl_a2_sfcsiz4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a2_sfgaex2_dims[0] = time_dim;
    ncl_a2_sfgaex2_dims[1] = lat_dim;
    ncl_a2_sfgaex2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a2_sfgaex2", NC_DOUBLE, RANK_ncl_a2_sfgaex2, ncl_a2_sfgaex2_dims, &ncl_a2_sfgaex2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2_sfgaex2_id, NC_CHUNKED, ncl_a2_sfgaex2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a3_dims[0] = time_dim;
    ncl_a3_dims[1] = lev_dim;
    ncl_a3_dims[2] = lat_dim;
    ncl_a3_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a3", NC_DOUBLE, RANK_ncl_a3, ncl_a3_dims, &ncl_a3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3_id, NC_CHUNKED, ncl_a3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a3DDF_dims[0] = time_dim;
    ncl_a3DDF_dims[1] = lat_dim;
    ncl_a3DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a3DDF", NC_DOUBLE, RANK_ncl_a3DDF, ncl_a3DDF_dims, &ncl_a3DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3DDF_id, NC_CHUNKED, ncl_a3DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a3GVF_dims[0] = time_dim;
    ncl_a3GVF_dims[1] = lat_dim;
    ncl_a3GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a3GVF", NC_DOUBLE, RANK_ncl_a3GVF, ncl_a3GVF_dims, &ncl_a3GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3GVF_id, NC_CHUNKED, ncl_a3GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a3SF_dims[0] = time_dim;
    ncl_a3SF_dims[1] = lat_dim;
    ncl_a3SF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a3SF", NC_DOUBLE, RANK_ncl_a3SF, ncl_a3SF_dims, &ncl_a3SF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SF_id, NC_CHUNKED, ncl_a3SF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a3SFSBC_dims[0] = time_dim;
    ncl_a3SFSBC_dims[1] = lat_dim;
    ncl_a3SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a3SFSBC", NC_DOUBLE, RANK_ncl_a3SFSBC, ncl_a3SFSBC_dims, &ncl_a3SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SFSBC_id, NC_CHUNKED, ncl_a3SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a3SFSBS_dims[0] = time_dim;
    ncl_a3SFSBS_dims[1] = lat_dim;
    ncl_a3SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a3SFSBS", NC_DOUBLE, RANK_ncl_a3SFSBS, ncl_a3SFSBS_dims, &ncl_a3SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SFSBS_id, NC_CHUNKED, ncl_a3SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a3SFSIC_dims[0] = time_dim;
    ncl_a3SFSIC_dims[1] = lat_dim;
    ncl_a3SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a3SFSIC", NC_DOUBLE, RANK_ncl_a3SFSIC, ncl_a3SFSIC_dims, &ncl_a3SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SFSIC_id, NC_CHUNKED, ncl_a3SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a3SFSIS_dims[0] = time_dim;
    ncl_a3SFSIS_dims[1] = lat_dim;
    ncl_a3SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a3SFSIS", NC_DOUBLE, RANK_ncl_a3SFSIS, ncl_a3SFSIS_dims, &ncl_a3SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SFSIS_id, NC_CHUNKED, ncl_a3SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a3SFWET_dims[0] = time_dim;
    ncl_a3SFWET_dims[1] = lat_dim;
    ncl_a3SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a3SFWET", NC_DOUBLE, RANK_ncl_a3SFWET, ncl_a3SFWET_dims, &ncl_a3SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SFWET_id, NC_CHUNKED, ncl_a3SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_a3TBF_dims[0] = time_dim;
    ncl_a3TBF_dims[1] = lat_dim;
    ncl_a3TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_a3TBF", NC_DOUBLE, RANK_ncl_a3TBF, ncl_a3TBF_dims, &ncl_a3TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3TBF_id, NC_CHUNKED, ncl_a3TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c1_dims[0] = time_dim;
    ncl_c1_dims[1] = lev_dim;
    ncl_c1_dims[2] = lat_dim;
    ncl_c1_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c1", NC_DOUBLE, RANK_ncl_c1, ncl_c1_dims, &ncl_c1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1_id, NC_CHUNKED, ncl_c1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c1DDF_dims[0] = time_dim;
    ncl_c1DDF_dims[1] = lat_dim;
    ncl_c1DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c1DDF", NC_DOUBLE, RANK_ncl_c1DDF, ncl_c1DDF_dims, &ncl_c1DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1DDF_id, NC_CHUNKED, ncl_c1DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c1GVF_dims[0] = time_dim;
    ncl_c1GVF_dims[1] = lat_dim;
    ncl_c1GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c1GVF", NC_DOUBLE, RANK_ncl_c1GVF, ncl_c1GVF_dims, &ncl_c1GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1GVF_id, NC_CHUNKED, ncl_c1GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c1SFSBC_dims[0] = time_dim;
    ncl_c1SFSBC_dims[1] = lat_dim;
    ncl_c1SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c1SFSBC", NC_DOUBLE, RANK_ncl_c1SFSBC, ncl_c1SFSBC_dims, &ncl_c1SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1SFSBC_id, NC_CHUNKED, ncl_c1SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c1SFSBS_dims[0] = time_dim;
    ncl_c1SFSBS_dims[1] = lat_dim;
    ncl_c1SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c1SFSBS", NC_DOUBLE, RANK_ncl_c1SFSBS, ncl_c1SFSBS_dims, &ncl_c1SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1SFSBS_id, NC_CHUNKED, ncl_c1SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c1SFSIC_dims[0] = time_dim;
    ncl_c1SFSIC_dims[1] = lat_dim;
    ncl_c1SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c1SFSIC", NC_DOUBLE, RANK_ncl_c1SFSIC, ncl_c1SFSIC_dims, &ncl_c1SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1SFSIC_id, NC_CHUNKED, ncl_c1SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c1SFSIS_dims[0] = time_dim;
    ncl_c1SFSIS_dims[1] = lat_dim;
    ncl_c1SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c1SFSIS", NC_DOUBLE, RANK_ncl_c1SFSIS, ncl_c1SFSIS_dims, &ncl_c1SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1SFSIS_id, NC_CHUNKED, ncl_c1SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c1SFWET_dims[0] = time_dim;
    ncl_c1SFWET_dims[1] = lat_dim;
    ncl_c1SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c1SFWET", NC_DOUBLE, RANK_ncl_c1SFWET, ncl_c1SFWET_dims, &ncl_c1SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1SFWET_id, NC_CHUNKED, ncl_c1SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c1TBF_dims[0] = time_dim;
    ncl_c1TBF_dims[1] = lat_dim;
    ncl_c1TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c1TBF", NC_DOUBLE, RANK_ncl_c1TBF, ncl_c1TBF_dims, &ncl_c1TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1TBF_id, NC_CHUNKED, ncl_c1TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c1_sfcsiz3_dims[0] = time_dim;
    ncl_c1_sfcsiz3_dims[1] = lat_dim;
    ncl_c1_sfcsiz3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c1_sfcsiz3", NC_DOUBLE, RANK_ncl_c1_sfcsiz3, ncl_c1_sfcsiz3_dims, &ncl_c1_sfcsiz3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1_sfcsiz3_id, NC_CHUNKED, ncl_c1_sfcsiz3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c1_sfcsiz4_dims[0] = time_dim;
    ncl_c1_sfcsiz4_dims[1] = lat_dim;
    ncl_c1_sfcsiz4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c1_sfcsiz4", NC_DOUBLE, RANK_ncl_c1_sfcsiz4, ncl_c1_sfcsiz4_dims, &ncl_c1_sfcsiz4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1_sfcsiz4_id, NC_CHUNKED, ncl_c1_sfcsiz4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c1_sfgaex2_dims[0] = time_dim;
    ncl_c1_sfgaex2_dims[1] = lat_dim;
    ncl_c1_sfgaex2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c1_sfgaex2", NC_DOUBLE, RANK_ncl_c1_sfgaex2, ncl_c1_sfgaex2_dims, &ncl_c1_sfgaex2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1_sfgaex2_id, NC_CHUNKED, ncl_c1_sfgaex2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c2_dims[0] = time_dim;
    ncl_c2_dims[1] = lev_dim;
    ncl_c2_dims[2] = lat_dim;
    ncl_c2_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c2", NC_DOUBLE, RANK_ncl_c2, ncl_c2_dims, &ncl_c2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2_id, NC_CHUNKED, ncl_c2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c2DDF_dims[0] = time_dim;
    ncl_c2DDF_dims[1] = lat_dim;
    ncl_c2DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c2DDF", NC_DOUBLE, RANK_ncl_c2DDF, ncl_c2DDF_dims, &ncl_c2DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2DDF_id, NC_CHUNKED, ncl_c2DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c2GVF_dims[0] = time_dim;
    ncl_c2GVF_dims[1] = lat_dim;
    ncl_c2GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c2GVF", NC_DOUBLE, RANK_ncl_c2GVF, ncl_c2GVF_dims, &ncl_c2GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2GVF_id, NC_CHUNKED, ncl_c2GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c2SFSBC_dims[0] = time_dim;
    ncl_c2SFSBC_dims[1] = lat_dim;
    ncl_c2SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c2SFSBC", NC_DOUBLE, RANK_ncl_c2SFSBC, ncl_c2SFSBC_dims, &ncl_c2SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2SFSBC_id, NC_CHUNKED, ncl_c2SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c2SFSBS_dims[0] = time_dim;
    ncl_c2SFSBS_dims[1] = lat_dim;
    ncl_c2SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c2SFSBS", NC_DOUBLE, RANK_ncl_c2SFSBS, ncl_c2SFSBS_dims, &ncl_c2SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2SFSBS_id, NC_CHUNKED, ncl_c2SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c2SFSIC_dims[0] = time_dim;
    ncl_c2SFSIC_dims[1] = lat_dim;
    ncl_c2SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c2SFSIC", NC_DOUBLE, RANK_ncl_c2SFSIC, ncl_c2SFSIC_dims, &ncl_c2SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2SFSIC_id, NC_CHUNKED, ncl_c2SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c2SFSIS_dims[0] = time_dim;
    ncl_c2SFSIS_dims[1] = lat_dim;
    ncl_c2SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c2SFSIS", NC_DOUBLE, RANK_ncl_c2SFSIS, ncl_c2SFSIS_dims, &ncl_c2SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2SFSIS_id, NC_CHUNKED, ncl_c2SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c2SFWET_dims[0] = time_dim;
    ncl_c2SFWET_dims[1] = lat_dim;
    ncl_c2SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c2SFWET", NC_DOUBLE, RANK_ncl_c2SFWET, ncl_c2SFWET_dims, &ncl_c2SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2SFWET_id, NC_CHUNKED, ncl_c2SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c2TBF_dims[0] = time_dim;
    ncl_c2TBF_dims[1] = lat_dim;
    ncl_c2TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c2TBF", NC_DOUBLE, RANK_ncl_c2TBF, ncl_c2TBF_dims, &ncl_c2TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2TBF_id, NC_CHUNKED, ncl_c2TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c2_sfcsiz3_dims[0] = time_dim;
    ncl_c2_sfcsiz3_dims[1] = lat_dim;
    ncl_c2_sfcsiz3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c2_sfcsiz3", NC_DOUBLE, RANK_ncl_c2_sfcsiz3, ncl_c2_sfcsiz3_dims, &ncl_c2_sfcsiz3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2_sfcsiz3_id, NC_CHUNKED, ncl_c2_sfcsiz3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c2_sfcsiz4_dims[0] = time_dim;
    ncl_c2_sfcsiz4_dims[1] = lat_dim;
    ncl_c2_sfcsiz4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c2_sfcsiz4", NC_DOUBLE, RANK_ncl_c2_sfcsiz4, ncl_c2_sfcsiz4_dims, &ncl_c2_sfcsiz4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2_sfcsiz4_id, NC_CHUNKED, ncl_c2_sfcsiz4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c2_sfgaex2_dims[0] = time_dim;
    ncl_c2_sfgaex2_dims[1] = lat_dim;
    ncl_c2_sfgaex2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c2_sfgaex2", NC_DOUBLE, RANK_ncl_c2_sfgaex2, ncl_c2_sfgaex2_dims, &ncl_c2_sfgaex2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2_sfgaex2_id, NC_CHUNKED, ncl_c2_sfgaex2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c3_dims[0] = time_dim;
    ncl_c3_dims[1] = lev_dim;
    ncl_c3_dims[2] = lat_dim;
    ncl_c3_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c3", NC_DOUBLE, RANK_ncl_c3, ncl_c3_dims, &ncl_c3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3_id, NC_CHUNKED, ncl_c3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c3DDF_dims[0] = time_dim;
    ncl_c3DDF_dims[1] = lat_dim;
    ncl_c3DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c3DDF", NC_DOUBLE, RANK_ncl_c3DDF, ncl_c3DDF_dims, &ncl_c3DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3DDF_id, NC_CHUNKED, ncl_c3DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c3GVF_dims[0] = time_dim;
    ncl_c3GVF_dims[1] = lat_dim;
    ncl_c3GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c3GVF", NC_DOUBLE, RANK_ncl_c3GVF, ncl_c3GVF_dims, &ncl_c3GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3GVF_id, NC_CHUNKED, ncl_c3GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c3SFSBC_dims[0] = time_dim;
    ncl_c3SFSBC_dims[1] = lat_dim;
    ncl_c3SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c3SFSBC", NC_DOUBLE, RANK_ncl_c3SFSBC, ncl_c3SFSBC_dims, &ncl_c3SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3SFSBC_id, NC_CHUNKED, ncl_c3SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c3SFSBS_dims[0] = time_dim;
    ncl_c3SFSBS_dims[1] = lat_dim;
    ncl_c3SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c3SFSBS", NC_DOUBLE, RANK_ncl_c3SFSBS, ncl_c3SFSBS_dims, &ncl_c3SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3SFSBS_id, NC_CHUNKED, ncl_c3SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c3SFSIC_dims[0] = time_dim;
    ncl_c3SFSIC_dims[1] = lat_dim;
    ncl_c3SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c3SFSIC", NC_DOUBLE, RANK_ncl_c3SFSIC, ncl_c3SFSIC_dims, &ncl_c3SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3SFSIC_id, NC_CHUNKED, ncl_c3SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c3SFSIS_dims[0] = time_dim;
    ncl_c3SFSIS_dims[1] = lat_dim;
    ncl_c3SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c3SFSIS", NC_DOUBLE, RANK_ncl_c3SFSIS, ncl_c3SFSIS_dims, &ncl_c3SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3SFSIS_id, NC_CHUNKED, ncl_c3SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c3SFWET_dims[0] = time_dim;
    ncl_c3SFWET_dims[1] = lat_dim;
    ncl_c3SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c3SFWET", NC_DOUBLE, RANK_ncl_c3SFWET, ncl_c3SFWET_dims, &ncl_c3SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3SFWET_id, NC_CHUNKED, ncl_c3SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    ncl_c3TBF_dims[0] = time_dim;
    ncl_c3TBF_dims[1] = lat_dim;
    ncl_c3TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "ncl_c3TBF", NC_DOUBLE, RANK_ncl_c3TBF, ncl_c3TBF_dims, &ncl_c3TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3TBF_id, NC_CHUNKED, ncl_c3TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a1_dims[0] = time_dim;
    num_a1_dims[1] = lev_dim;
    num_a1_dims[2] = lat_dim;
    num_a1_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a1", NC_DOUBLE, RANK_num_a1, num_a1_dims, &num_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_id, NC_CHUNKED, num_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a1DDF_dims[0] = time_dim;
    num_a1DDF_dims[1] = lat_dim;
    num_a1DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a1DDF", NC_DOUBLE, RANK_num_a1DDF, num_a1DDF_dims, &num_a1DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1DDF_id, NC_CHUNKED, num_a1DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a1GVF_dims[0] = time_dim;
    num_a1GVF_dims[1] = lat_dim;
    num_a1GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a1GVF", NC_DOUBLE, RANK_num_a1GVF, num_a1GVF_dims, &num_a1GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1GVF_id, NC_CHUNKED, num_a1GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a1SFSBC_dims[0] = time_dim;
    num_a1SFSBC_dims[1] = lat_dim;
    num_a1SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a1SFSBC", NC_DOUBLE, RANK_num_a1SFSBC, num_a1SFSBC_dims, &num_a1SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1SFSBC_id, NC_CHUNKED, num_a1SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a1SFSBS_dims[0] = time_dim;
    num_a1SFSBS_dims[1] = lat_dim;
    num_a1SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a1SFSBS", NC_DOUBLE, RANK_num_a1SFSBS, num_a1SFSBS_dims, &num_a1SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1SFSBS_id, NC_CHUNKED, num_a1SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a1SFSIC_dims[0] = time_dim;
    num_a1SFSIC_dims[1] = lat_dim;
    num_a1SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a1SFSIC", NC_DOUBLE, RANK_num_a1SFSIC, num_a1SFSIC_dims, &num_a1SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1SFSIC_id, NC_CHUNKED, num_a1SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a1SFSIS_dims[0] = time_dim;
    num_a1SFSIS_dims[1] = lat_dim;
    num_a1SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a1SFSIS", NC_DOUBLE, RANK_num_a1SFSIS, num_a1SFSIS_dims, &num_a1SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1SFSIS_id, NC_CHUNKED, num_a1SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a1SFWET_dims[0] = time_dim;
    num_a1SFWET_dims[1] = lat_dim;
    num_a1SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a1SFWET", NC_DOUBLE, RANK_num_a1SFWET, num_a1SFWET_dims, &num_a1SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1SFWET_id, NC_CHUNKED, num_a1SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a1TBF_dims[0] = time_dim;
    num_a1TBF_dims[1] = lat_dim;
    num_a1TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a1TBF", NC_DOUBLE, RANK_num_a1TBF, num_a1TBF_dims, &num_a1TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1TBF_id, NC_CHUNKED, num_a1TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a1_CLXF_dims[0] = time_dim;
    num_a1_CLXF_dims[1] = lat_dim;
    num_a1_CLXF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a1_CLXF", NC_DOUBLE, RANK_num_a1_CLXF, num_a1_CLXF_dims, &num_a1_CLXF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_CLXF_id, NC_CHUNKED, num_a1_CLXF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a1_XFRC_dims[0] = time_dim;
    num_a1_XFRC_dims[1] = lev_dim;
    num_a1_XFRC_dims[2] = lat_dim;
    num_a1_XFRC_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a1_XFRC", NC_DOUBLE, RANK_num_a1_XFRC, num_a1_XFRC_dims, &num_a1_XFRC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_XFRC_id, NC_CHUNKED, num_a1_XFRC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a1_sfcoag1_dims[0] = time_dim;
    num_a1_sfcoag1_dims[1] = lat_dim;
    num_a1_sfcoag1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a1_sfcoag1", NC_DOUBLE, RANK_num_a1_sfcoag1, num_a1_sfcoag1_dims, &num_a1_sfcoag1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfcoag1_id, NC_CHUNKED, num_a1_sfcoag1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a1_sfcsiz1_dims[0] = time_dim;
    num_a1_sfcsiz1_dims[1] = lat_dim;
    num_a1_sfcsiz1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a1_sfcsiz1", NC_DOUBLE, RANK_num_a1_sfcsiz1, num_a1_sfcsiz1_dims, &num_a1_sfcsiz1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfcsiz1_id, NC_CHUNKED, num_a1_sfcsiz1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a1_sfcsiz2_dims[0] = time_dim;
    num_a1_sfcsiz2_dims[1] = lat_dim;
    num_a1_sfcsiz2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a1_sfcsiz2", NC_DOUBLE, RANK_num_a1_sfcsiz2, num_a1_sfcsiz2_dims, &num_a1_sfcsiz2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfcsiz2_id, NC_CHUNKED, num_a1_sfcsiz2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a1_sfcsiz3_dims[0] = time_dim;
    num_a1_sfcsiz3_dims[1] = lat_dim;
    num_a1_sfcsiz3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a1_sfcsiz3", NC_DOUBLE, RANK_num_a1_sfcsiz3, num_a1_sfcsiz3_dims, &num_a1_sfcsiz3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfcsiz3_id, NC_CHUNKED, num_a1_sfcsiz3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a1_sfcsiz4_dims[0] = time_dim;
    num_a1_sfcsiz4_dims[1] = lat_dim;
    num_a1_sfcsiz4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a1_sfcsiz4", NC_DOUBLE, RANK_num_a1_sfcsiz4, num_a1_sfcsiz4_dims, &num_a1_sfcsiz4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfcsiz4_id, NC_CHUNKED, num_a1_sfcsiz4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a1_sfgaex2_dims[0] = time_dim;
    num_a1_sfgaex2_dims[1] = lat_dim;
    num_a1_sfgaex2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a1_sfgaex2", NC_DOUBLE, RANK_num_a1_sfgaex2, num_a1_sfgaex2_dims, &num_a1_sfgaex2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfgaex2_id, NC_CHUNKED, num_a1_sfgaex2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a2_dims[0] = time_dim;
    num_a2_dims[1] = lev_dim;
    num_a2_dims[2] = lat_dim;
    num_a2_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a2", NC_DOUBLE, RANK_num_a2, num_a2_dims, &num_a2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_id, NC_CHUNKED, num_a2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a2DDF_dims[0] = time_dim;
    num_a2DDF_dims[1] = lat_dim;
    num_a2DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a2DDF", NC_DOUBLE, RANK_num_a2DDF, num_a2DDF_dims, &num_a2DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2DDF_id, NC_CHUNKED, num_a2DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a2GVF_dims[0] = time_dim;
    num_a2GVF_dims[1] = lat_dim;
    num_a2GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a2GVF", NC_DOUBLE, RANK_num_a2GVF, num_a2GVF_dims, &num_a2GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2GVF_id, NC_CHUNKED, num_a2GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a2SFSBC_dims[0] = time_dim;
    num_a2SFSBC_dims[1] = lat_dim;
    num_a2SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a2SFSBC", NC_DOUBLE, RANK_num_a2SFSBC, num_a2SFSBC_dims, &num_a2SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2SFSBC_id, NC_CHUNKED, num_a2SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a2SFSBS_dims[0] = time_dim;
    num_a2SFSBS_dims[1] = lat_dim;
    num_a2SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a2SFSBS", NC_DOUBLE, RANK_num_a2SFSBS, num_a2SFSBS_dims, &num_a2SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2SFSBS_id, NC_CHUNKED, num_a2SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a2SFSIC_dims[0] = time_dim;
    num_a2SFSIC_dims[1] = lat_dim;
    num_a2SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a2SFSIC", NC_DOUBLE, RANK_num_a2SFSIC, num_a2SFSIC_dims, &num_a2SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2SFSIC_id, NC_CHUNKED, num_a2SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a2SFSIS_dims[0] = time_dim;
    num_a2SFSIS_dims[1] = lat_dim;
    num_a2SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a2SFSIS", NC_DOUBLE, RANK_num_a2SFSIS, num_a2SFSIS_dims, &num_a2SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2SFSIS_id, NC_CHUNKED, num_a2SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a2SFWET_dims[0] = time_dim;
    num_a2SFWET_dims[1] = lat_dim;
    num_a2SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a2SFWET", NC_DOUBLE, RANK_num_a2SFWET, num_a2SFWET_dims, &num_a2SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2SFWET_id, NC_CHUNKED, num_a2SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a2TBF_dims[0] = time_dim;
    num_a2TBF_dims[1] = lat_dim;
    num_a2TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a2TBF", NC_DOUBLE, RANK_num_a2TBF, num_a2TBF_dims, &num_a2TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2TBF_id, NC_CHUNKED, num_a2TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a2_CLXF_dims[0] = time_dim;
    num_a2_CLXF_dims[1] = lat_dim;
    num_a2_CLXF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a2_CLXF", NC_DOUBLE, RANK_num_a2_CLXF, num_a2_CLXF_dims, &num_a2_CLXF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_CLXF_id, NC_CHUNKED, num_a2_CLXF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a2_XFRC_dims[0] = time_dim;
    num_a2_XFRC_dims[1] = lev_dim;
    num_a2_XFRC_dims[2] = lat_dim;
    num_a2_XFRC_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a2_XFRC", NC_DOUBLE, RANK_num_a2_XFRC, num_a2_XFRC_dims, &num_a2_XFRC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_XFRC_id, NC_CHUNKED, num_a2_XFRC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a2_sfcoag1_dims[0] = time_dim;
    num_a2_sfcoag1_dims[1] = lat_dim;
    num_a2_sfcoag1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a2_sfcoag1", NC_DOUBLE, RANK_num_a2_sfcoag1, num_a2_sfcoag1_dims, &num_a2_sfcoag1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfcoag1_id, NC_CHUNKED, num_a2_sfcoag1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a2_sfcsiz1_dims[0] = time_dim;
    num_a2_sfcsiz1_dims[1] = lat_dim;
    num_a2_sfcsiz1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a2_sfcsiz1", NC_DOUBLE, RANK_num_a2_sfcsiz1, num_a2_sfcsiz1_dims, &num_a2_sfcsiz1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfcsiz1_id, NC_CHUNKED, num_a2_sfcsiz1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a2_sfcsiz2_dims[0] = time_dim;
    num_a2_sfcsiz2_dims[1] = lat_dim;
    num_a2_sfcsiz2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a2_sfcsiz2", NC_DOUBLE, RANK_num_a2_sfcsiz2, num_a2_sfcsiz2_dims, &num_a2_sfcsiz2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfcsiz2_id, NC_CHUNKED, num_a2_sfcsiz2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a2_sfcsiz3_dims[0] = time_dim;
    num_a2_sfcsiz3_dims[1] = lat_dim;
    num_a2_sfcsiz3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a2_sfcsiz3", NC_DOUBLE, RANK_num_a2_sfcsiz3, num_a2_sfcsiz3_dims, &num_a2_sfcsiz3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfcsiz3_id, NC_CHUNKED, num_a2_sfcsiz3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a2_sfcsiz4_dims[0] = time_dim;
    num_a2_sfcsiz4_dims[1] = lat_dim;
    num_a2_sfcsiz4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a2_sfcsiz4", NC_DOUBLE, RANK_num_a2_sfcsiz4, num_a2_sfcsiz4_dims, &num_a2_sfcsiz4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfcsiz4_id, NC_CHUNKED, num_a2_sfcsiz4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a2_sfgaex2_dims[0] = time_dim;
    num_a2_sfgaex2_dims[1] = lat_dim;
    num_a2_sfgaex2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a2_sfgaex2", NC_DOUBLE, RANK_num_a2_sfgaex2, num_a2_sfgaex2_dims, &num_a2_sfgaex2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfgaex2_id, NC_CHUNKED, num_a2_sfgaex2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a2_sfnnuc1_dims[0] = time_dim;
    num_a2_sfnnuc1_dims[1] = lat_dim;
    num_a2_sfnnuc1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a2_sfnnuc1", NC_DOUBLE, RANK_num_a2_sfnnuc1, num_a2_sfnnuc1_dims, &num_a2_sfnnuc1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfnnuc1_id, NC_CHUNKED, num_a2_sfnnuc1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a3_dims[0] = time_dim;
    num_a3_dims[1] = lev_dim;
    num_a3_dims[2] = lat_dim;
    num_a3_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a3", NC_DOUBLE, RANK_num_a3, num_a3_dims, &num_a3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3_id, NC_CHUNKED, num_a3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a3DDF_dims[0] = time_dim;
    num_a3DDF_dims[1] = lat_dim;
    num_a3DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a3DDF", NC_DOUBLE, RANK_num_a3DDF, num_a3DDF_dims, &num_a3DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3DDF_id, NC_CHUNKED, num_a3DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a3GVF_dims[0] = time_dim;
    num_a3GVF_dims[1] = lat_dim;
    num_a3GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a3GVF", NC_DOUBLE, RANK_num_a3GVF, num_a3GVF_dims, &num_a3GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3GVF_id, NC_CHUNKED, num_a3GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a3SFSBC_dims[0] = time_dim;
    num_a3SFSBC_dims[1] = lat_dim;
    num_a3SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a3SFSBC", NC_DOUBLE, RANK_num_a3SFSBC, num_a3SFSBC_dims, &num_a3SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3SFSBC_id, NC_CHUNKED, num_a3SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a3SFSBS_dims[0] = time_dim;
    num_a3SFSBS_dims[1] = lat_dim;
    num_a3SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a3SFSBS", NC_DOUBLE, RANK_num_a3SFSBS, num_a3SFSBS_dims, &num_a3SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3SFSBS_id, NC_CHUNKED, num_a3SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a3SFSIC_dims[0] = time_dim;
    num_a3SFSIC_dims[1] = lat_dim;
    num_a3SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a3SFSIC", NC_DOUBLE, RANK_num_a3SFSIC, num_a3SFSIC_dims, &num_a3SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3SFSIC_id, NC_CHUNKED, num_a3SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a3SFSIS_dims[0] = time_dim;
    num_a3SFSIS_dims[1] = lat_dim;
    num_a3SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a3SFSIS", NC_DOUBLE, RANK_num_a3SFSIS, num_a3SFSIS_dims, &num_a3SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3SFSIS_id, NC_CHUNKED, num_a3SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a3SFWET_dims[0] = time_dim;
    num_a3SFWET_dims[1] = lat_dim;
    num_a3SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a3SFWET", NC_DOUBLE, RANK_num_a3SFWET, num_a3SFWET_dims, &num_a3SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3SFWET_id, NC_CHUNKED, num_a3SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a3TBF_dims[0] = time_dim;
    num_a3TBF_dims[1] = lat_dim;
    num_a3TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a3TBF", NC_DOUBLE, RANK_num_a3TBF, num_a3TBF_dims, &num_a3TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3TBF_id, NC_CHUNKED, num_a3TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a3_sfcsiz1_dims[0] = time_dim;
    num_a3_sfcsiz1_dims[1] = lat_dim;
    num_a3_sfcsiz1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a3_sfcsiz1", NC_DOUBLE, RANK_num_a3_sfcsiz1, num_a3_sfcsiz1_dims, &num_a3_sfcsiz1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3_sfcsiz1_id, NC_CHUNKED, num_a3_sfcsiz1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_a3_sfcsiz2_dims[0] = time_dim;
    num_a3_sfcsiz2_dims[1] = lat_dim;
    num_a3_sfcsiz2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_a3_sfcsiz2", NC_DOUBLE, RANK_num_a3_sfcsiz2, num_a3_sfcsiz2_dims, &num_a3_sfcsiz2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3_sfcsiz2_id, NC_CHUNKED, num_a3_sfcsiz2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c1_dims[0] = time_dim;
    num_c1_dims[1] = lev_dim;
    num_c1_dims[2] = lat_dim;
    num_c1_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c1", NC_DOUBLE, RANK_num_c1, num_c1_dims, &num_c1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_id, NC_CHUNKED, num_c1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c1DDF_dims[0] = time_dim;
    num_c1DDF_dims[1] = lat_dim;
    num_c1DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c1DDF", NC_DOUBLE, RANK_num_c1DDF, num_c1DDF_dims, &num_c1DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1DDF_id, NC_CHUNKED, num_c1DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c1GVF_dims[0] = time_dim;
    num_c1GVF_dims[1] = lat_dim;
    num_c1GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c1GVF", NC_DOUBLE, RANK_num_c1GVF, num_c1GVF_dims, &num_c1GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1GVF_id, NC_CHUNKED, num_c1GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c1SFSBC_dims[0] = time_dim;
    num_c1SFSBC_dims[1] = lat_dim;
    num_c1SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c1SFSBC", NC_DOUBLE, RANK_num_c1SFSBC, num_c1SFSBC_dims, &num_c1SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1SFSBC_id, NC_CHUNKED, num_c1SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c1SFSBS_dims[0] = time_dim;
    num_c1SFSBS_dims[1] = lat_dim;
    num_c1SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c1SFSBS", NC_DOUBLE, RANK_num_c1SFSBS, num_c1SFSBS_dims, &num_c1SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1SFSBS_id, NC_CHUNKED, num_c1SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c1SFSIC_dims[0] = time_dim;
    num_c1SFSIC_dims[1] = lat_dim;
    num_c1SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c1SFSIC", NC_DOUBLE, RANK_num_c1SFSIC, num_c1SFSIC_dims, &num_c1SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1SFSIC_id, NC_CHUNKED, num_c1SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c1SFSIS_dims[0] = time_dim;
    num_c1SFSIS_dims[1] = lat_dim;
    num_c1SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c1SFSIS", NC_DOUBLE, RANK_num_c1SFSIS, num_c1SFSIS_dims, &num_c1SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1SFSIS_id, NC_CHUNKED, num_c1SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c1SFWET_dims[0] = time_dim;
    num_c1SFWET_dims[1] = lat_dim;
    num_c1SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c1SFWET", NC_DOUBLE, RANK_num_c1SFWET, num_c1SFWET_dims, &num_c1SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1SFWET_id, NC_CHUNKED, num_c1SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c1TBF_dims[0] = time_dim;
    num_c1TBF_dims[1] = lat_dim;
    num_c1TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c1TBF", NC_DOUBLE, RANK_num_c1TBF, num_c1TBF_dims, &num_c1TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1TBF_id, NC_CHUNKED, num_c1TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c1_sfcsiz1_dims[0] = time_dim;
    num_c1_sfcsiz1_dims[1] = lat_dim;
    num_c1_sfcsiz1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c1_sfcsiz1", NC_DOUBLE, RANK_num_c1_sfcsiz1, num_c1_sfcsiz1_dims, &num_c1_sfcsiz1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_sfcsiz1_id, NC_CHUNKED, num_c1_sfcsiz1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c1_sfcsiz2_dims[0] = time_dim;
    num_c1_sfcsiz2_dims[1] = lat_dim;
    num_c1_sfcsiz2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c1_sfcsiz2", NC_DOUBLE, RANK_num_c1_sfcsiz2, num_c1_sfcsiz2_dims, &num_c1_sfcsiz2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_sfcsiz2_id, NC_CHUNKED, num_c1_sfcsiz2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c1_sfcsiz3_dims[0] = time_dim;
    num_c1_sfcsiz3_dims[1] = lat_dim;
    num_c1_sfcsiz3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c1_sfcsiz3", NC_DOUBLE, RANK_num_c1_sfcsiz3, num_c1_sfcsiz3_dims, &num_c1_sfcsiz3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_sfcsiz3_id, NC_CHUNKED, num_c1_sfcsiz3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c1_sfcsiz4_dims[0] = time_dim;
    num_c1_sfcsiz4_dims[1] = lat_dim;
    num_c1_sfcsiz4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c1_sfcsiz4", NC_DOUBLE, RANK_num_c1_sfcsiz4, num_c1_sfcsiz4_dims, &num_c1_sfcsiz4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_sfcsiz4_id, NC_CHUNKED, num_c1_sfcsiz4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c1_sfgaex2_dims[0] = time_dim;
    num_c1_sfgaex2_dims[1] = lat_dim;
    num_c1_sfgaex2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c1_sfgaex2", NC_DOUBLE, RANK_num_c1_sfgaex2, num_c1_sfgaex2_dims, &num_c1_sfgaex2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_sfgaex2_id, NC_CHUNKED, num_c1_sfgaex2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c2_dims[0] = time_dim;
    num_c2_dims[1] = lev_dim;
    num_c2_dims[2] = lat_dim;
    num_c2_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c2", NC_DOUBLE, RANK_num_c2, num_c2_dims, &num_c2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_id, NC_CHUNKED, num_c2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c2DDF_dims[0] = time_dim;
    num_c2DDF_dims[1] = lat_dim;
    num_c2DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c2DDF", NC_DOUBLE, RANK_num_c2DDF, num_c2DDF_dims, &num_c2DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2DDF_id, NC_CHUNKED, num_c2DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c2GVF_dims[0] = time_dim;
    num_c2GVF_dims[1] = lat_dim;
    num_c2GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c2GVF", NC_DOUBLE, RANK_num_c2GVF, num_c2GVF_dims, &num_c2GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2GVF_id, NC_CHUNKED, num_c2GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c2SFSBC_dims[0] = time_dim;
    num_c2SFSBC_dims[1] = lat_dim;
    num_c2SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c2SFSBC", NC_DOUBLE, RANK_num_c2SFSBC, num_c2SFSBC_dims, &num_c2SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2SFSBC_id, NC_CHUNKED, num_c2SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c2SFSBS_dims[0] = time_dim;
    num_c2SFSBS_dims[1] = lat_dim;
    num_c2SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c2SFSBS", NC_DOUBLE, RANK_num_c2SFSBS, num_c2SFSBS_dims, &num_c2SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2SFSBS_id, NC_CHUNKED, num_c2SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c2SFSIC_dims[0] = time_dim;
    num_c2SFSIC_dims[1] = lat_dim;
    num_c2SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c2SFSIC", NC_DOUBLE, RANK_num_c2SFSIC, num_c2SFSIC_dims, &num_c2SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2SFSIC_id, NC_CHUNKED, num_c2SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c2SFSIS_dims[0] = time_dim;
    num_c2SFSIS_dims[1] = lat_dim;
    num_c2SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c2SFSIS", NC_DOUBLE, RANK_num_c2SFSIS, num_c2SFSIS_dims, &num_c2SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2SFSIS_id, NC_CHUNKED, num_c2SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c2SFWET_dims[0] = time_dim;
    num_c2SFWET_dims[1] = lat_dim;
    num_c2SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c2SFWET", NC_DOUBLE, RANK_num_c2SFWET, num_c2SFWET_dims, &num_c2SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2SFWET_id, NC_CHUNKED, num_c2SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c2TBF_dims[0] = time_dim;
    num_c2TBF_dims[1] = lat_dim;
    num_c2TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c2TBF", NC_DOUBLE, RANK_num_c2TBF, num_c2TBF_dims, &num_c2TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2TBF_id, NC_CHUNKED, num_c2TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c2_sfcsiz1_dims[0] = time_dim;
    num_c2_sfcsiz1_dims[1] = lat_dim;
    num_c2_sfcsiz1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c2_sfcsiz1", NC_DOUBLE, RANK_num_c2_sfcsiz1, num_c2_sfcsiz1_dims, &num_c2_sfcsiz1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_sfcsiz1_id, NC_CHUNKED, num_c2_sfcsiz1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c2_sfcsiz2_dims[0] = time_dim;
    num_c2_sfcsiz2_dims[1] = lat_dim;
    num_c2_sfcsiz2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c2_sfcsiz2", NC_DOUBLE, RANK_num_c2_sfcsiz2, num_c2_sfcsiz2_dims, &num_c2_sfcsiz2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_sfcsiz2_id, NC_CHUNKED, num_c2_sfcsiz2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c2_sfcsiz3_dims[0] = time_dim;
    num_c2_sfcsiz3_dims[1] = lat_dim;
    num_c2_sfcsiz3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c2_sfcsiz3", NC_DOUBLE, RANK_num_c2_sfcsiz3, num_c2_sfcsiz3_dims, &num_c2_sfcsiz3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_sfcsiz3_id, NC_CHUNKED, num_c2_sfcsiz3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c2_sfcsiz4_dims[0] = time_dim;
    num_c2_sfcsiz4_dims[1] = lat_dim;
    num_c2_sfcsiz4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c2_sfcsiz4", NC_DOUBLE, RANK_num_c2_sfcsiz4, num_c2_sfcsiz4_dims, &num_c2_sfcsiz4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_sfcsiz4_id, NC_CHUNKED, num_c2_sfcsiz4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c2_sfgaex2_dims[0] = time_dim;
    num_c2_sfgaex2_dims[1] = lat_dim;
    num_c2_sfgaex2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c2_sfgaex2", NC_DOUBLE, RANK_num_c2_sfgaex2, num_c2_sfgaex2_dims, &num_c2_sfgaex2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_sfgaex2_id, NC_CHUNKED, num_c2_sfgaex2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c3_dims[0] = time_dim;
    num_c3_dims[1] = lev_dim;
    num_c3_dims[2] = lat_dim;
    num_c3_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c3", NC_DOUBLE, RANK_num_c3, num_c3_dims, &num_c3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3_id, NC_CHUNKED, num_c3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c3DDF_dims[0] = time_dim;
    num_c3DDF_dims[1] = lat_dim;
    num_c3DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c3DDF", NC_DOUBLE, RANK_num_c3DDF, num_c3DDF_dims, &num_c3DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3DDF_id, NC_CHUNKED, num_c3DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c3GVF_dims[0] = time_dim;
    num_c3GVF_dims[1] = lat_dim;
    num_c3GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c3GVF", NC_DOUBLE, RANK_num_c3GVF, num_c3GVF_dims, &num_c3GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3GVF_id, NC_CHUNKED, num_c3GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c3SFSBC_dims[0] = time_dim;
    num_c3SFSBC_dims[1] = lat_dim;
    num_c3SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c3SFSBC", NC_DOUBLE, RANK_num_c3SFSBC, num_c3SFSBC_dims, &num_c3SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3SFSBC_id, NC_CHUNKED, num_c3SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c3SFSBS_dims[0] = time_dim;
    num_c3SFSBS_dims[1] = lat_dim;
    num_c3SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c3SFSBS", NC_DOUBLE, RANK_num_c3SFSBS, num_c3SFSBS_dims, &num_c3SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3SFSBS_id, NC_CHUNKED, num_c3SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c3SFSIC_dims[0] = time_dim;
    num_c3SFSIC_dims[1] = lat_dim;
    num_c3SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c3SFSIC", NC_DOUBLE, RANK_num_c3SFSIC, num_c3SFSIC_dims, &num_c3SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3SFSIC_id, NC_CHUNKED, num_c3SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c3SFSIS_dims[0] = time_dim;
    num_c3SFSIS_dims[1] = lat_dim;
    num_c3SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c3SFSIS", NC_DOUBLE, RANK_num_c3SFSIS, num_c3SFSIS_dims, &num_c3SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3SFSIS_id, NC_CHUNKED, num_c3SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c3SFWET_dims[0] = time_dim;
    num_c3SFWET_dims[1] = lat_dim;
    num_c3SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c3SFWET", NC_DOUBLE, RANK_num_c3SFWET, num_c3SFWET_dims, &num_c3SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3SFWET_id, NC_CHUNKED, num_c3SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c3TBF_dims[0] = time_dim;
    num_c3TBF_dims[1] = lat_dim;
    num_c3TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c3TBF", NC_DOUBLE, RANK_num_c3TBF, num_c3TBF_dims, &num_c3TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3TBF_id, NC_CHUNKED, num_c3TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c3_sfcsiz1_dims[0] = time_dim;
    num_c3_sfcsiz1_dims[1] = lat_dim;
    num_c3_sfcsiz1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c3_sfcsiz1", NC_DOUBLE, RANK_num_c3_sfcsiz1, num_c3_sfcsiz1_dims, &num_c3_sfcsiz1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3_sfcsiz1_id, NC_CHUNKED, num_c3_sfcsiz1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    num_c3_sfcsiz2_dims[0] = time_dim;
    num_c3_sfcsiz2_dims[1] = lat_dim;
    num_c3_sfcsiz2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "num_c3_sfcsiz2", NC_DOUBLE, RANK_num_c3_sfcsiz2, num_c3_sfcsiz2_dims, &num_c3_sfcsiz2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3_sfcsiz2_id, NC_CHUNKED, num_c3_sfcsiz2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    pom_a1_dims[0] = time_dim;
    pom_a1_dims[1] = lev_dim;
    pom_a1_dims[2] = lat_dim;
    pom_a1_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "pom_a1", NC_DOUBLE, RANK_pom_a1, pom_a1_dims, &pom_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1_id, NC_CHUNKED, pom_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    pom_a1DDF_dims[0] = time_dim;
    pom_a1DDF_dims[1] = lat_dim;
    pom_a1DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "pom_a1DDF", NC_DOUBLE, RANK_pom_a1DDF, pom_a1DDF_dims, &pom_a1DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1DDF_id, NC_CHUNKED, pom_a1DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    pom_a1GVF_dims[0] = time_dim;
    pom_a1GVF_dims[1] = lat_dim;
    pom_a1GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "pom_a1GVF", NC_DOUBLE, RANK_pom_a1GVF, pom_a1GVF_dims, &pom_a1GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1GVF_id, NC_CHUNKED, pom_a1GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    pom_a1SFSBC_dims[0] = time_dim;
    pom_a1SFSBC_dims[1] = lat_dim;
    pom_a1SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "pom_a1SFSBC", NC_DOUBLE, RANK_pom_a1SFSBC, pom_a1SFSBC_dims, &pom_a1SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1SFSBC_id, NC_CHUNKED, pom_a1SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    pom_a1SFSBS_dims[0] = time_dim;
    pom_a1SFSBS_dims[1] = lat_dim;
    pom_a1SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "pom_a1SFSBS", NC_DOUBLE, RANK_pom_a1SFSBS, pom_a1SFSBS_dims, &pom_a1SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1SFSBS_id, NC_CHUNKED, pom_a1SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    pom_a1SFSIC_dims[0] = time_dim;
    pom_a1SFSIC_dims[1] = lat_dim;
    pom_a1SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "pom_a1SFSIC", NC_DOUBLE, RANK_pom_a1SFSIC, pom_a1SFSIC_dims, &pom_a1SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1SFSIC_id, NC_CHUNKED, pom_a1SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    pom_a1SFSIS_dims[0] = time_dim;
    pom_a1SFSIS_dims[1] = lat_dim;
    pom_a1SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "pom_a1SFSIS", NC_DOUBLE, RANK_pom_a1SFSIS, pom_a1SFSIS_dims, &pom_a1SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1SFSIS_id, NC_CHUNKED, pom_a1SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    pom_a1SFWET_dims[0] = time_dim;
    pom_a1SFWET_dims[1] = lat_dim;
    pom_a1SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "pom_a1SFWET", NC_DOUBLE, RANK_pom_a1SFWET, pom_a1SFWET_dims, &pom_a1SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1SFWET_id, NC_CHUNKED, pom_a1SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    pom_a1TBF_dims[0] = time_dim;
    pom_a1TBF_dims[1] = lat_dim;
    pom_a1TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "pom_a1TBF", NC_DOUBLE, RANK_pom_a1TBF, pom_a1TBF_dims, &pom_a1TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1TBF_id, NC_CHUNKED, pom_a1TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    pom_a1_CLXF_dims[0] = time_dim;
    pom_a1_CLXF_dims[1] = lat_dim;
    pom_a1_CLXF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "pom_a1_CLXF", NC_DOUBLE, RANK_pom_a1_CLXF, pom_a1_CLXF_dims, &pom_a1_CLXF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1_CLXF_id, NC_CHUNKED, pom_a1_CLXF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    pom_a1_XFRC_dims[0] = time_dim;
    pom_a1_XFRC_dims[1] = lev_dim;
    pom_a1_XFRC_dims[2] = lat_dim;
    pom_a1_XFRC_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "pom_a1_XFRC", NC_DOUBLE, RANK_pom_a1_XFRC, pom_a1_XFRC_dims, &pom_a1_XFRC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1_XFRC_id, NC_CHUNKED, pom_a1_XFRC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    pom_c1_dims[0] = time_dim;
    pom_c1_dims[1] = lev_dim;
    pom_c1_dims[2] = lat_dim;
    pom_c1_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "pom_c1", NC_DOUBLE, RANK_pom_c1, pom_c1_dims, &pom_c1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1_id, NC_CHUNKED, pom_c1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    pom_c1DDF_dims[0] = time_dim;
    pom_c1DDF_dims[1] = lat_dim;
    pom_c1DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "pom_c1DDF", NC_DOUBLE, RANK_pom_c1DDF, pom_c1DDF_dims, &pom_c1DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1DDF_id, NC_CHUNKED, pom_c1DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    pom_c1GVF_dims[0] = time_dim;
    pom_c1GVF_dims[1] = lat_dim;
    pom_c1GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "pom_c1GVF", NC_DOUBLE, RANK_pom_c1GVF, pom_c1GVF_dims, &pom_c1GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1GVF_id, NC_CHUNKED, pom_c1GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    pom_c1SFSBC_dims[0] = time_dim;
    pom_c1SFSBC_dims[1] = lat_dim;
    pom_c1SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "pom_c1SFSBC", NC_DOUBLE, RANK_pom_c1SFSBC, pom_c1SFSBC_dims, &pom_c1SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1SFSBC_id, NC_CHUNKED, pom_c1SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    pom_c1SFSBS_dims[0] = time_dim;
    pom_c1SFSBS_dims[1] = lat_dim;
    pom_c1SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "pom_c1SFSBS", NC_DOUBLE, RANK_pom_c1SFSBS, pom_c1SFSBS_dims, &pom_c1SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1SFSBS_id, NC_CHUNKED, pom_c1SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    pom_c1SFSIC_dims[0] = time_dim;
    pom_c1SFSIC_dims[1] = lat_dim;
    pom_c1SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "pom_c1SFSIC", NC_DOUBLE, RANK_pom_c1SFSIC, pom_c1SFSIC_dims, &pom_c1SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1SFSIC_id, NC_CHUNKED, pom_c1SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    pom_c1SFSIS_dims[0] = time_dim;
    pom_c1SFSIS_dims[1] = lat_dim;
    pom_c1SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "pom_c1SFSIS", NC_DOUBLE, RANK_pom_c1SFSIS, pom_c1SFSIS_dims, &pom_c1SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1SFSIS_id, NC_CHUNKED, pom_c1SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    pom_c1SFWET_dims[0] = time_dim;
    pom_c1SFWET_dims[1] = lat_dim;
    pom_c1SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "pom_c1SFWET", NC_DOUBLE, RANK_pom_c1SFWET, pom_c1SFWET_dims, &pom_c1SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1SFWET_id, NC_CHUNKED, pom_c1SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    pom_c1TBF_dims[0] = time_dim;
    pom_c1TBF_dims[1] = lat_dim;
    pom_c1TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "pom_c1TBF", NC_DOUBLE, RANK_pom_c1TBF, pom_c1TBF_dims, &pom_c1TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1TBF_id, NC_CHUNKED, pom_c1TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a1_dims[0] = time_dim;
    so4_a1_dims[1] = lev_dim;
    so4_a1_dims[2] = lat_dim;
    so4_a1_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a1", NC_DOUBLE, RANK_so4_a1, so4_a1_dims, &so4_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_id, NC_CHUNKED, so4_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a1DDF_dims[0] = time_dim;
    so4_a1DDF_dims[1] = lat_dim;
    so4_a1DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a1DDF", NC_DOUBLE, RANK_so4_a1DDF, so4_a1DDF_dims, &so4_a1DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1DDF_id, NC_CHUNKED, so4_a1DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a1GVF_dims[0] = time_dim;
    so4_a1GVF_dims[1] = lat_dim;
    so4_a1GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a1GVF", NC_DOUBLE, RANK_so4_a1GVF, so4_a1GVF_dims, &so4_a1GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1GVF_id, NC_CHUNKED, so4_a1GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a1SFSBC_dims[0] = time_dim;
    so4_a1SFSBC_dims[1] = lat_dim;
    so4_a1SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a1SFSBC", NC_DOUBLE, RANK_so4_a1SFSBC, so4_a1SFSBC_dims, &so4_a1SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1SFSBC_id, NC_CHUNKED, so4_a1SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a1SFSBS_dims[0] = time_dim;
    so4_a1SFSBS_dims[1] = lat_dim;
    so4_a1SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a1SFSBS", NC_DOUBLE, RANK_so4_a1SFSBS, so4_a1SFSBS_dims, &so4_a1SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1SFSBS_id, NC_CHUNKED, so4_a1SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a1SFSIC_dims[0] = time_dim;
    so4_a1SFSIC_dims[1] = lat_dim;
    so4_a1SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a1SFSIC", NC_DOUBLE, RANK_so4_a1SFSIC, so4_a1SFSIC_dims, &so4_a1SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1SFSIC_id, NC_CHUNKED, so4_a1SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a1SFSIS_dims[0] = time_dim;
    so4_a1SFSIS_dims[1] = lat_dim;
    so4_a1SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a1SFSIS", NC_DOUBLE, RANK_so4_a1SFSIS, so4_a1SFSIS_dims, &so4_a1SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1SFSIS_id, NC_CHUNKED, so4_a1SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a1SFWET_dims[0] = time_dim;
    so4_a1SFWET_dims[1] = lat_dim;
    so4_a1SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a1SFWET", NC_DOUBLE, RANK_so4_a1SFWET, so4_a1SFWET_dims, &so4_a1SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1SFWET_id, NC_CHUNKED, so4_a1SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a1TBF_dims[0] = time_dim;
    so4_a1TBF_dims[1] = lat_dim;
    so4_a1TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a1TBF", NC_DOUBLE, RANK_so4_a1TBF, so4_a1TBF_dims, &so4_a1TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1TBF_id, NC_CHUNKED, so4_a1TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a1_CLXF_dims[0] = time_dim;
    so4_a1_CLXF_dims[1] = lat_dim;
    so4_a1_CLXF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a1_CLXF", NC_DOUBLE, RANK_so4_a1_CLXF, so4_a1_CLXF_dims, &so4_a1_CLXF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_CLXF_id, NC_CHUNKED, so4_a1_CLXF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a1_XFRC_dims[0] = time_dim;
    so4_a1_XFRC_dims[1] = lev_dim;
    so4_a1_XFRC_dims[2] = lat_dim;
    so4_a1_XFRC_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a1_XFRC", NC_DOUBLE, RANK_so4_a1_XFRC, so4_a1_XFRC_dims, &so4_a1_XFRC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_XFRC_id, NC_CHUNKED, so4_a1_XFRC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a1_sfcoag1_dims[0] = time_dim;
    so4_a1_sfcoag1_dims[1] = lat_dim;
    so4_a1_sfcoag1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a1_sfcoag1", NC_DOUBLE, RANK_so4_a1_sfcoag1, so4_a1_sfcoag1_dims, &so4_a1_sfcoag1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_sfcoag1_id, NC_CHUNKED, so4_a1_sfcoag1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a1_sfcsiz3_dims[0] = time_dim;
    so4_a1_sfcsiz3_dims[1] = lat_dim;
    so4_a1_sfcsiz3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a1_sfcsiz3", NC_DOUBLE, RANK_so4_a1_sfcsiz3, so4_a1_sfcsiz3_dims, &so4_a1_sfcsiz3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_sfcsiz3_id, NC_CHUNKED, so4_a1_sfcsiz3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a1_sfcsiz4_dims[0] = time_dim;
    so4_a1_sfcsiz4_dims[1] = lat_dim;
    so4_a1_sfcsiz4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a1_sfcsiz4", NC_DOUBLE, RANK_so4_a1_sfcsiz4, so4_a1_sfcsiz4_dims, &so4_a1_sfcsiz4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_sfcsiz4_id, NC_CHUNKED, so4_a1_sfcsiz4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a1_sfgaex1_dims[0] = time_dim;
    so4_a1_sfgaex1_dims[1] = lat_dim;
    so4_a1_sfgaex1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a1_sfgaex1", NC_DOUBLE, RANK_so4_a1_sfgaex1, so4_a1_sfgaex1_dims, &so4_a1_sfgaex1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_sfgaex1_id, NC_CHUNKED, so4_a1_sfgaex1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a1_sfgaex2_dims[0] = time_dim;
    so4_a1_sfgaex2_dims[1] = lat_dim;
    so4_a1_sfgaex2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a1_sfgaex2", NC_DOUBLE, RANK_so4_a1_sfgaex2, so4_a1_sfgaex2_dims, &so4_a1_sfgaex2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_sfgaex2_id, NC_CHUNKED, so4_a1_sfgaex2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a2_dims[0] = time_dim;
    so4_a2_dims[1] = lev_dim;
    so4_a2_dims[2] = lat_dim;
    so4_a2_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a2", NC_DOUBLE, RANK_so4_a2, so4_a2_dims, &so4_a2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_id, NC_CHUNKED, so4_a2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a2DDF_dims[0] = time_dim;
    so4_a2DDF_dims[1] = lat_dim;
    so4_a2DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a2DDF", NC_DOUBLE, RANK_so4_a2DDF, so4_a2DDF_dims, &so4_a2DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2DDF_id, NC_CHUNKED, so4_a2DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a2GVF_dims[0] = time_dim;
    so4_a2GVF_dims[1] = lat_dim;
    so4_a2GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a2GVF", NC_DOUBLE, RANK_so4_a2GVF, so4_a2GVF_dims, &so4_a2GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2GVF_id, NC_CHUNKED, so4_a2GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a2SFSBC_dims[0] = time_dim;
    so4_a2SFSBC_dims[1] = lat_dim;
    so4_a2SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a2SFSBC", NC_DOUBLE, RANK_so4_a2SFSBC, so4_a2SFSBC_dims, &so4_a2SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2SFSBC_id, NC_CHUNKED, so4_a2SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a2SFSBS_dims[0] = time_dim;
    so4_a2SFSBS_dims[1] = lat_dim;
    so4_a2SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a2SFSBS", NC_DOUBLE, RANK_so4_a2SFSBS, so4_a2SFSBS_dims, &so4_a2SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2SFSBS_id, NC_CHUNKED, so4_a2SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a2SFSIC_dims[0] = time_dim;
    so4_a2SFSIC_dims[1] = lat_dim;
    so4_a2SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a2SFSIC", NC_DOUBLE, RANK_so4_a2SFSIC, so4_a2SFSIC_dims, &so4_a2SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2SFSIC_id, NC_CHUNKED, so4_a2SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a2SFSIS_dims[0] = time_dim;
    so4_a2SFSIS_dims[1] = lat_dim;
    so4_a2SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a2SFSIS", NC_DOUBLE, RANK_so4_a2SFSIS, so4_a2SFSIS_dims, &so4_a2SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2SFSIS_id, NC_CHUNKED, so4_a2SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a2SFWET_dims[0] = time_dim;
    so4_a2SFWET_dims[1] = lat_dim;
    so4_a2SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a2SFWET", NC_DOUBLE, RANK_so4_a2SFWET, so4_a2SFWET_dims, &so4_a2SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2SFWET_id, NC_CHUNKED, so4_a2SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a2TBF_dims[0] = time_dim;
    so4_a2TBF_dims[1] = lat_dim;
    so4_a2TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a2TBF", NC_DOUBLE, RANK_so4_a2TBF, so4_a2TBF_dims, &so4_a2TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2TBF_id, NC_CHUNKED, so4_a2TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a2_CLXF_dims[0] = time_dim;
    so4_a2_CLXF_dims[1] = lat_dim;
    so4_a2_CLXF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a2_CLXF", NC_DOUBLE, RANK_so4_a2_CLXF, so4_a2_CLXF_dims, &so4_a2_CLXF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_CLXF_id, NC_CHUNKED, so4_a2_CLXF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a2_XFRC_dims[0] = time_dim;
    so4_a2_XFRC_dims[1] = lev_dim;
    so4_a2_XFRC_dims[2] = lat_dim;
    so4_a2_XFRC_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a2_XFRC", NC_DOUBLE, RANK_so4_a2_XFRC, so4_a2_XFRC_dims, &so4_a2_XFRC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_XFRC_id, NC_CHUNKED, so4_a2_XFRC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a2_sfcoag1_dims[0] = time_dim;
    so4_a2_sfcoag1_dims[1] = lat_dim;
    so4_a2_sfcoag1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a2_sfcoag1", NC_DOUBLE, RANK_so4_a2_sfcoag1, so4_a2_sfcoag1_dims, &so4_a2_sfcoag1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfcoag1_id, NC_CHUNKED, so4_a2_sfcoag1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a2_sfcsiz3_dims[0] = time_dim;
    so4_a2_sfcsiz3_dims[1] = lat_dim;
    so4_a2_sfcsiz3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a2_sfcsiz3", NC_DOUBLE, RANK_so4_a2_sfcsiz3, so4_a2_sfcsiz3_dims, &so4_a2_sfcsiz3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfcsiz3_id, NC_CHUNKED, so4_a2_sfcsiz3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a2_sfcsiz4_dims[0] = time_dim;
    so4_a2_sfcsiz4_dims[1] = lat_dim;
    so4_a2_sfcsiz4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a2_sfcsiz4", NC_DOUBLE, RANK_so4_a2_sfcsiz4, so4_a2_sfcsiz4_dims, &so4_a2_sfcsiz4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfcsiz4_id, NC_CHUNKED, so4_a2_sfcsiz4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a2_sfgaex1_dims[0] = time_dim;
    so4_a2_sfgaex1_dims[1] = lat_dim;
    so4_a2_sfgaex1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a2_sfgaex1", NC_DOUBLE, RANK_so4_a2_sfgaex1, so4_a2_sfgaex1_dims, &so4_a2_sfgaex1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfgaex1_id, NC_CHUNKED, so4_a2_sfgaex1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a2_sfgaex2_dims[0] = time_dim;
    so4_a2_sfgaex2_dims[1] = lat_dim;
    so4_a2_sfgaex2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a2_sfgaex2", NC_DOUBLE, RANK_so4_a2_sfgaex2, so4_a2_sfgaex2_dims, &so4_a2_sfgaex2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfgaex2_id, NC_CHUNKED, so4_a2_sfgaex2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a2_sfnnuc1_dims[0] = time_dim;
    so4_a2_sfnnuc1_dims[1] = lat_dim;
    so4_a2_sfnnuc1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a2_sfnnuc1", NC_DOUBLE, RANK_so4_a2_sfnnuc1, so4_a2_sfnnuc1_dims, &so4_a2_sfnnuc1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfnnuc1_id, NC_CHUNKED, so4_a2_sfnnuc1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a3_dims[0] = time_dim;
    so4_a3_dims[1] = lev_dim;
    so4_a3_dims[2] = lat_dim;
    so4_a3_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a3", NC_DOUBLE, RANK_so4_a3, so4_a3_dims, &so4_a3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3_id, NC_CHUNKED, so4_a3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a3DDF_dims[0] = time_dim;
    so4_a3DDF_dims[1] = lat_dim;
    so4_a3DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a3DDF", NC_DOUBLE, RANK_so4_a3DDF, so4_a3DDF_dims, &so4_a3DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3DDF_id, NC_CHUNKED, so4_a3DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a3GVF_dims[0] = time_dim;
    so4_a3GVF_dims[1] = lat_dim;
    so4_a3GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a3GVF", NC_DOUBLE, RANK_so4_a3GVF, so4_a3GVF_dims, &so4_a3GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3GVF_id, NC_CHUNKED, so4_a3GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a3SFSBC_dims[0] = time_dim;
    so4_a3SFSBC_dims[1] = lat_dim;
    so4_a3SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a3SFSBC", NC_DOUBLE, RANK_so4_a3SFSBC, so4_a3SFSBC_dims, &so4_a3SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3SFSBC_id, NC_CHUNKED, so4_a3SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a3SFSBS_dims[0] = time_dim;
    so4_a3SFSBS_dims[1] = lat_dim;
    so4_a3SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a3SFSBS", NC_DOUBLE, RANK_so4_a3SFSBS, so4_a3SFSBS_dims, &so4_a3SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3SFSBS_id, NC_CHUNKED, so4_a3SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a3SFSIC_dims[0] = time_dim;
    so4_a3SFSIC_dims[1] = lat_dim;
    so4_a3SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a3SFSIC", NC_DOUBLE, RANK_so4_a3SFSIC, so4_a3SFSIC_dims, &so4_a3SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3SFSIC_id, NC_CHUNKED, so4_a3SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a3SFSIS_dims[0] = time_dim;
    so4_a3SFSIS_dims[1] = lat_dim;
    so4_a3SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a3SFSIS", NC_DOUBLE, RANK_so4_a3SFSIS, so4_a3SFSIS_dims, &so4_a3SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3SFSIS_id, NC_CHUNKED, so4_a3SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a3SFWET_dims[0] = time_dim;
    so4_a3SFWET_dims[1] = lat_dim;
    so4_a3SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a3SFWET", NC_DOUBLE, RANK_so4_a3SFWET, so4_a3SFWET_dims, &so4_a3SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3SFWET_id, NC_CHUNKED, so4_a3SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a3TBF_dims[0] = time_dim;
    so4_a3TBF_dims[1] = lat_dim;
    so4_a3TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a3TBF", NC_DOUBLE, RANK_so4_a3TBF, so4_a3TBF_dims, &so4_a3TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3TBF_id, NC_CHUNKED, so4_a3TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_a3_sfgaex1_dims[0] = time_dim;
    so4_a3_sfgaex1_dims[1] = lat_dim;
    so4_a3_sfgaex1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_a3_sfgaex1", NC_DOUBLE, RANK_so4_a3_sfgaex1, so4_a3_sfgaex1_dims, &so4_a3_sfgaex1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3_sfgaex1_id, NC_CHUNKED, so4_a3_sfgaex1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c1_dims[0] = time_dim;
    so4_c1_dims[1] = lev_dim;
    so4_c1_dims[2] = lat_dim;
    so4_c1_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c1", NC_DOUBLE, RANK_so4_c1, so4_c1_dims, &so4_c1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1_id, NC_CHUNKED, so4_c1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c1AQH2SO4_dims[0] = time_dim;
    so4_c1AQH2SO4_dims[1] = lat_dim;
    so4_c1AQH2SO4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c1AQH2SO4", NC_DOUBLE, RANK_so4_c1AQH2SO4, so4_c1AQH2SO4_dims, &so4_c1AQH2SO4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1AQH2SO4_id, NC_CHUNKED, so4_c1AQH2SO4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c1AQSO4_dims[0] = time_dim;
    so4_c1AQSO4_dims[1] = lat_dim;
    so4_c1AQSO4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c1AQSO4", NC_DOUBLE, RANK_so4_c1AQSO4, so4_c1AQSO4_dims, &so4_c1AQSO4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1AQSO4_id, NC_CHUNKED, so4_c1AQSO4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c1DDF_dims[0] = time_dim;
    so4_c1DDF_dims[1] = lat_dim;
    so4_c1DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c1DDF", NC_DOUBLE, RANK_so4_c1DDF, so4_c1DDF_dims, &so4_c1DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1DDF_id, NC_CHUNKED, so4_c1DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c1GVF_dims[0] = time_dim;
    so4_c1GVF_dims[1] = lat_dim;
    so4_c1GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c1GVF", NC_DOUBLE, RANK_so4_c1GVF, so4_c1GVF_dims, &so4_c1GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1GVF_id, NC_CHUNKED, so4_c1GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c1SFSBC_dims[0] = time_dim;
    so4_c1SFSBC_dims[1] = lat_dim;
    so4_c1SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c1SFSBC", NC_DOUBLE, RANK_so4_c1SFSBC, so4_c1SFSBC_dims, &so4_c1SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1SFSBC_id, NC_CHUNKED, so4_c1SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c1SFSBS_dims[0] = time_dim;
    so4_c1SFSBS_dims[1] = lat_dim;
    so4_c1SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c1SFSBS", NC_DOUBLE, RANK_so4_c1SFSBS, so4_c1SFSBS_dims, &so4_c1SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1SFSBS_id, NC_CHUNKED, so4_c1SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c1SFSIC_dims[0] = time_dim;
    so4_c1SFSIC_dims[1] = lat_dim;
    so4_c1SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c1SFSIC", NC_DOUBLE, RANK_so4_c1SFSIC, so4_c1SFSIC_dims, &so4_c1SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1SFSIC_id, NC_CHUNKED, so4_c1SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c1SFSIS_dims[0] = time_dim;
    so4_c1SFSIS_dims[1] = lat_dim;
    so4_c1SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c1SFSIS", NC_DOUBLE, RANK_so4_c1SFSIS, so4_c1SFSIS_dims, &so4_c1SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1SFSIS_id, NC_CHUNKED, so4_c1SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c1SFWET_dims[0] = time_dim;
    so4_c1SFWET_dims[1] = lat_dim;
    so4_c1SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c1SFWET", NC_DOUBLE, RANK_so4_c1SFWET, so4_c1SFWET_dims, &so4_c1SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1SFWET_id, NC_CHUNKED, so4_c1SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c1TBF_dims[0] = time_dim;
    so4_c1TBF_dims[1] = lat_dim;
    so4_c1TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c1TBF", NC_DOUBLE, RANK_so4_c1TBF, so4_c1TBF_dims, &so4_c1TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1TBF_id, NC_CHUNKED, so4_c1TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c1_sfcsiz3_dims[0] = time_dim;
    so4_c1_sfcsiz3_dims[1] = lat_dim;
    so4_c1_sfcsiz3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c1_sfcsiz3", NC_DOUBLE, RANK_so4_c1_sfcsiz3, so4_c1_sfcsiz3_dims, &so4_c1_sfcsiz3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1_sfcsiz3_id, NC_CHUNKED, so4_c1_sfcsiz3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c1_sfcsiz4_dims[0] = time_dim;
    so4_c1_sfcsiz4_dims[1] = lat_dim;
    so4_c1_sfcsiz4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c1_sfcsiz4", NC_DOUBLE, RANK_so4_c1_sfcsiz4, so4_c1_sfcsiz4_dims, &so4_c1_sfcsiz4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1_sfcsiz4_id, NC_CHUNKED, so4_c1_sfcsiz4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c1_sfgaex2_dims[0] = time_dim;
    so4_c1_sfgaex2_dims[1] = lat_dim;
    so4_c1_sfgaex2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c1_sfgaex2", NC_DOUBLE, RANK_so4_c1_sfgaex2, so4_c1_sfgaex2_dims, &so4_c1_sfgaex2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1_sfgaex2_id, NC_CHUNKED, so4_c1_sfgaex2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c2_dims[0] = time_dim;
    so4_c2_dims[1] = lev_dim;
    so4_c2_dims[2] = lat_dim;
    so4_c2_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c2", NC_DOUBLE, RANK_so4_c2, so4_c2_dims, &so4_c2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2_id, NC_CHUNKED, so4_c2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c2AQH2SO4_dims[0] = time_dim;
    so4_c2AQH2SO4_dims[1] = lat_dim;
    so4_c2AQH2SO4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c2AQH2SO4", NC_DOUBLE, RANK_so4_c2AQH2SO4, so4_c2AQH2SO4_dims, &so4_c2AQH2SO4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2AQH2SO4_id, NC_CHUNKED, so4_c2AQH2SO4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c2AQSO4_dims[0] = time_dim;
    so4_c2AQSO4_dims[1] = lat_dim;
    so4_c2AQSO4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c2AQSO4", NC_DOUBLE, RANK_so4_c2AQSO4, so4_c2AQSO4_dims, &so4_c2AQSO4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2AQSO4_id, NC_CHUNKED, so4_c2AQSO4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c2DDF_dims[0] = time_dim;
    so4_c2DDF_dims[1] = lat_dim;
    so4_c2DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c2DDF", NC_DOUBLE, RANK_so4_c2DDF, so4_c2DDF_dims, &so4_c2DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2DDF_id, NC_CHUNKED, so4_c2DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c2GVF_dims[0] = time_dim;
    so4_c2GVF_dims[1] = lat_dim;
    so4_c2GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c2GVF", NC_DOUBLE, RANK_so4_c2GVF, so4_c2GVF_dims, &so4_c2GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2GVF_id, NC_CHUNKED, so4_c2GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c2SFSBC_dims[0] = time_dim;
    so4_c2SFSBC_dims[1] = lat_dim;
    so4_c2SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c2SFSBC", NC_DOUBLE, RANK_so4_c2SFSBC, so4_c2SFSBC_dims, &so4_c2SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2SFSBC_id, NC_CHUNKED, so4_c2SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c2SFSBS_dims[0] = time_dim;
    so4_c2SFSBS_dims[1] = lat_dim;
    so4_c2SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c2SFSBS", NC_DOUBLE, RANK_so4_c2SFSBS, so4_c2SFSBS_dims, &so4_c2SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2SFSBS_id, NC_CHUNKED, so4_c2SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c2SFSIC_dims[0] = time_dim;
    so4_c2SFSIC_dims[1] = lat_dim;
    so4_c2SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c2SFSIC", NC_DOUBLE, RANK_so4_c2SFSIC, so4_c2SFSIC_dims, &so4_c2SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2SFSIC_id, NC_CHUNKED, so4_c2SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c2SFSIS_dims[0] = time_dim;
    so4_c2SFSIS_dims[1] = lat_dim;
    so4_c2SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c2SFSIS", NC_DOUBLE, RANK_so4_c2SFSIS, so4_c2SFSIS_dims, &so4_c2SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2SFSIS_id, NC_CHUNKED, so4_c2SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c2SFWET_dims[0] = time_dim;
    so4_c2SFWET_dims[1] = lat_dim;
    so4_c2SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c2SFWET", NC_DOUBLE, RANK_so4_c2SFWET, so4_c2SFWET_dims, &so4_c2SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2SFWET_id, NC_CHUNKED, so4_c2SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c2TBF_dims[0] = time_dim;
    so4_c2TBF_dims[1] = lat_dim;
    so4_c2TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c2TBF", NC_DOUBLE, RANK_so4_c2TBF, so4_c2TBF_dims, &so4_c2TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2TBF_id, NC_CHUNKED, so4_c2TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c2_sfcsiz3_dims[0] = time_dim;
    so4_c2_sfcsiz3_dims[1] = lat_dim;
    so4_c2_sfcsiz3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c2_sfcsiz3", NC_DOUBLE, RANK_so4_c2_sfcsiz3, so4_c2_sfcsiz3_dims, &so4_c2_sfcsiz3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2_sfcsiz3_id, NC_CHUNKED, so4_c2_sfcsiz3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c2_sfcsiz4_dims[0] = time_dim;
    so4_c2_sfcsiz4_dims[1] = lat_dim;
    so4_c2_sfcsiz4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c2_sfcsiz4", NC_DOUBLE, RANK_so4_c2_sfcsiz4, so4_c2_sfcsiz4_dims, &so4_c2_sfcsiz4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2_sfcsiz4_id, NC_CHUNKED, so4_c2_sfcsiz4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c2_sfgaex2_dims[0] = time_dim;
    so4_c2_sfgaex2_dims[1] = lat_dim;
    so4_c2_sfgaex2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c2_sfgaex2", NC_DOUBLE, RANK_so4_c2_sfgaex2, so4_c2_sfgaex2_dims, &so4_c2_sfgaex2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2_sfgaex2_id, NC_CHUNKED, so4_c2_sfgaex2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c3_dims[0] = time_dim;
    so4_c3_dims[1] = lev_dim;
    so4_c3_dims[2] = lat_dim;
    so4_c3_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c3", NC_DOUBLE, RANK_so4_c3, so4_c3_dims, &so4_c3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3_id, NC_CHUNKED, so4_c3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c3AQH2SO4_dims[0] = time_dim;
    so4_c3AQH2SO4_dims[1] = lat_dim;
    so4_c3AQH2SO4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c3AQH2SO4", NC_DOUBLE, RANK_so4_c3AQH2SO4, so4_c3AQH2SO4_dims, &so4_c3AQH2SO4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3AQH2SO4_id, NC_CHUNKED, so4_c3AQH2SO4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c3AQSO4_dims[0] = time_dim;
    so4_c3AQSO4_dims[1] = lat_dim;
    so4_c3AQSO4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c3AQSO4", NC_DOUBLE, RANK_so4_c3AQSO4, so4_c3AQSO4_dims, &so4_c3AQSO4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3AQSO4_id, NC_CHUNKED, so4_c3AQSO4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c3DDF_dims[0] = time_dim;
    so4_c3DDF_dims[1] = lat_dim;
    so4_c3DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c3DDF", NC_DOUBLE, RANK_so4_c3DDF, so4_c3DDF_dims, &so4_c3DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3DDF_id, NC_CHUNKED, so4_c3DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c3GVF_dims[0] = time_dim;
    so4_c3GVF_dims[1] = lat_dim;
    so4_c3GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c3GVF", NC_DOUBLE, RANK_so4_c3GVF, so4_c3GVF_dims, &so4_c3GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3GVF_id, NC_CHUNKED, so4_c3GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c3SFSBC_dims[0] = time_dim;
    so4_c3SFSBC_dims[1] = lat_dim;
    so4_c3SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c3SFSBC", NC_DOUBLE, RANK_so4_c3SFSBC, so4_c3SFSBC_dims, &so4_c3SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3SFSBC_id, NC_CHUNKED, so4_c3SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c3SFSBS_dims[0] = time_dim;
    so4_c3SFSBS_dims[1] = lat_dim;
    so4_c3SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c3SFSBS", NC_DOUBLE, RANK_so4_c3SFSBS, so4_c3SFSBS_dims, &so4_c3SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3SFSBS_id, NC_CHUNKED, so4_c3SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c3SFSIC_dims[0] = time_dim;
    so4_c3SFSIC_dims[1] = lat_dim;
    so4_c3SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c3SFSIC", NC_DOUBLE, RANK_so4_c3SFSIC, so4_c3SFSIC_dims, &so4_c3SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3SFSIC_id, NC_CHUNKED, so4_c3SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c3SFSIS_dims[0] = time_dim;
    so4_c3SFSIS_dims[1] = lat_dim;
    so4_c3SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c3SFSIS", NC_DOUBLE, RANK_so4_c3SFSIS, so4_c3SFSIS_dims, &so4_c3SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3SFSIS_id, NC_CHUNKED, so4_c3SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c3SFWET_dims[0] = time_dim;
    so4_c3SFWET_dims[1] = lat_dim;
    so4_c3SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c3SFWET", NC_DOUBLE, RANK_so4_c3SFWET, so4_c3SFWET_dims, &so4_c3SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3SFWET_id, NC_CHUNKED, so4_c3SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    so4_c3TBF_dims[0] = time_dim;
    so4_c3TBF_dims[1] = lat_dim;
    so4_c3TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "so4_c3TBF", NC_DOUBLE, RANK_so4_c3TBF, so4_c3TBF_dims, &so4_c3TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3TBF_id, NC_CHUNKED, so4_c3TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a1_dims[0] = time_dim;
    soa_a1_dims[1] = lev_dim;
    soa_a1_dims[2] = lat_dim;
    soa_a1_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a1", NC_DOUBLE, RANK_soa_a1, soa_a1_dims, &soa_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_id, NC_CHUNKED, soa_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a1DDF_dims[0] = time_dim;
    soa_a1DDF_dims[1] = lat_dim;
    soa_a1DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a1DDF", NC_DOUBLE, RANK_soa_a1DDF, soa_a1DDF_dims, &soa_a1DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1DDF_id, NC_CHUNKED, soa_a1DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a1GVF_dims[0] = time_dim;
    soa_a1GVF_dims[1] = lat_dim;
    soa_a1GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a1GVF", NC_DOUBLE, RANK_soa_a1GVF, soa_a1GVF_dims, &soa_a1GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1GVF_id, NC_CHUNKED, soa_a1GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a1SFSBC_dims[0] = time_dim;
    soa_a1SFSBC_dims[1] = lat_dim;
    soa_a1SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a1SFSBC", NC_DOUBLE, RANK_soa_a1SFSBC, soa_a1SFSBC_dims, &soa_a1SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1SFSBC_id, NC_CHUNKED, soa_a1SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a1SFSBS_dims[0] = time_dim;
    soa_a1SFSBS_dims[1] = lat_dim;
    soa_a1SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a1SFSBS", NC_DOUBLE, RANK_soa_a1SFSBS, soa_a1SFSBS_dims, &soa_a1SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1SFSBS_id, NC_CHUNKED, soa_a1SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a1SFSIC_dims[0] = time_dim;
    soa_a1SFSIC_dims[1] = lat_dim;
    soa_a1SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a1SFSIC", NC_DOUBLE, RANK_soa_a1SFSIC, soa_a1SFSIC_dims, &soa_a1SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1SFSIC_id, NC_CHUNKED, soa_a1SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a1SFSIS_dims[0] = time_dim;
    soa_a1SFSIS_dims[1] = lat_dim;
    soa_a1SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a1SFSIS", NC_DOUBLE, RANK_soa_a1SFSIS, soa_a1SFSIS_dims, &soa_a1SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1SFSIS_id, NC_CHUNKED, soa_a1SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a1SFWET_dims[0] = time_dim;
    soa_a1SFWET_dims[1] = lat_dim;
    soa_a1SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a1SFWET", NC_DOUBLE, RANK_soa_a1SFWET, soa_a1SFWET_dims, &soa_a1SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1SFWET_id, NC_CHUNKED, soa_a1SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a1TBF_dims[0] = time_dim;
    soa_a1TBF_dims[1] = lat_dim;
    soa_a1TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a1TBF", NC_DOUBLE, RANK_soa_a1TBF, soa_a1TBF_dims, &soa_a1TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1TBF_id, NC_CHUNKED, soa_a1TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a1_sfcoag1_dims[0] = time_dim;
    soa_a1_sfcoag1_dims[1] = lat_dim;
    soa_a1_sfcoag1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a1_sfcoag1", NC_DOUBLE, RANK_soa_a1_sfcoag1, soa_a1_sfcoag1_dims, &soa_a1_sfcoag1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_sfcoag1_id, NC_CHUNKED, soa_a1_sfcoag1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a1_sfcsiz3_dims[0] = time_dim;
    soa_a1_sfcsiz3_dims[1] = lat_dim;
    soa_a1_sfcsiz3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a1_sfcsiz3", NC_DOUBLE, RANK_soa_a1_sfcsiz3, soa_a1_sfcsiz3_dims, &soa_a1_sfcsiz3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_sfcsiz3_id, NC_CHUNKED, soa_a1_sfcsiz3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a1_sfcsiz4_dims[0] = time_dim;
    soa_a1_sfcsiz4_dims[1] = lat_dim;
    soa_a1_sfcsiz4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a1_sfcsiz4", NC_DOUBLE, RANK_soa_a1_sfcsiz4, soa_a1_sfcsiz4_dims, &soa_a1_sfcsiz4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_sfcsiz4_id, NC_CHUNKED, soa_a1_sfcsiz4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a1_sfgaex1_dims[0] = time_dim;
    soa_a1_sfgaex1_dims[1] = lat_dim;
    soa_a1_sfgaex1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a1_sfgaex1", NC_DOUBLE, RANK_soa_a1_sfgaex1, soa_a1_sfgaex1_dims, &soa_a1_sfgaex1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_sfgaex1_id, NC_CHUNKED, soa_a1_sfgaex1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a1_sfgaex2_dims[0] = time_dim;
    soa_a1_sfgaex2_dims[1] = lat_dim;
    soa_a1_sfgaex2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a1_sfgaex2", NC_DOUBLE, RANK_soa_a1_sfgaex2, soa_a1_sfgaex2_dims, &soa_a1_sfgaex2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_sfgaex2_id, NC_CHUNKED, soa_a1_sfgaex2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a2_dims[0] = time_dim;
    soa_a2_dims[1] = lev_dim;
    soa_a2_dims[2] = lat_dim;
    soa_a2_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a2", NC_DOUBLE, RANK_soa_a2, soa_a2_dims, &soa_a2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_id, NC_CHUNKED, soa_a2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a2DDF_dims[0] = time_dim;
    soa_a2DDF_dims[1] = lat_dim;
    soa_a2DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a2DDF", NC_DOUBLE, RANK_soa_a2DDF, soa_a2DDF_dims, &soa_a2DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2DDF_id, NC_CHUNKED, soa_a2DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a2GVF_dims[0] = time_dim;
    soa_a2GVF_dims[1] = lat_dim;
    soa_a2GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a2GVF", NC_DOUBLE, RANK_soa_a2GVF, soa_a2GVF_dims, &soa_a2GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2GVF_id, NC_CHUNKED, soa_a2GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a2SFSBC_dims[0] = time_dim;
    soa_a2SFSBC_dims[1] = lat_dim;
    soa_a2SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a2SFSBC", NC_DOUBLE, RANK_soa_a2SFSBC, soa_a2SFSBC_dims, &soa_a2SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2SFSBC_id, NC_CHUNKED, soa_a2SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a2SFSBS_dims[0] = time_dim;
    soa_a2SFSBS_dims[1] = lat_dim;
    soa_a2SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a2SFSBS", NC_DOUBLE, RANK_soa_a2SFSBS, soa_a2SFSBS_dims, &soa_a2SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2SFSBS_id, NC_CHUNKED, soa_a2SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a2SFSIC_dims[0] = time_dim;
    soa_a2SFSIC_dims[1] = lat_dim;
    soa_a2SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a2SFSIC", NC_DOUBLE, RANK_soa_a2SFSIC, soa_a2SFSIC_dims, &soa_a2SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2SFSIC_id, NC_CHUNKED, soa_a2SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a2SFSIS_dims[0] = time_dim;
    soa_a2SFSIS_dims[1] = lat_dim;
    soa_a2SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a2SFSIS", NC_DOUBLE, RANK_soa_a2SFSIS, soa_a2SFSIS_dims, &soa_a2SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2SFSIS_id, NC_CHUNKED, soa_a2SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a2SFWET_dims[0] = time_dim;
    soa_a2SFWET_dims[1] = lat_dim;
    soa_a2SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a2SFWET", NC_DOUBLE, RANK_soa_a2SFWET, soa_a2SFWET_dims, &soa_a2SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2SFWET_id, NC_CHUNKED, soa_a2SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a2TBF_dims[0] = time_dim;
    soa_a2TBF_dims[1] = lat_dim;
    soa_a2TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a2TBF", NC_DOUBLE, RANK_soa_a2TBF, soa_a2TBF_dims, &soa_a2TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2TBF_id, NC_CHUNKED, soa_a2TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a2_sfcoag1_dims[0] = time_dim;
    soa_a2_sfcoag1_dims[1] = lat_dim;
    soa_a2_sfcoag1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a2_sfcoag1", NC_DOUBLE, RANK_soa_a2_sfcoag1, soa_a2_sfcoag1_dims, &soa_a2_sfcoag1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_sfcoag1_id, NC_CHUNKED, soa_a2_sfcoag1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a2_sfcsiz3_dims[0] = time_dim;
    soa_a2_sfcsiz3_dims[1] = lat_dim;
    soa_a2_sfcsiz3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a2_sfcsiz3", NC_DOUBLE, RANK_soa_a2_sfcsiz3, soa_a2_sfcsiz3_dims, &soa_a2_sfcsiz3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_sfcsiz3_id, NC_CHUNKED, soa_a2_sfcsiz3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a2_sfcsiz4_dims[0] = time_dim;
    soa_a2_sfcsiz4_dims[1] = lat_dim;
    soa_a2_sfcsiz4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a2_sfcsiz4", NC_DOUBLE, RANK_soa_a2_sfcsiz4, soa_a2_sfcsiz4_dims, &soa_a2_sfcsiz4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_sfcsiz4_id, NC_CHUNKED, soa_a2_sfcsiz4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a2_sfgaex1_dims[0] = time_dim;
    soa_a2_sfgaex1_dims[1] = lat_dim;
    soa_a2_sfgaex1_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a2_sfgaex1", NC_DOUBLE, RANK_soa_a2_sfgaex1, soa_a2_sfgaex1_dims, &soa_a2_sfgaex1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_sfgaex1_id, NC_CHUNKED, soa_a2_sfgaex1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_a2_sfgaex2_dims[0] = time_dim;
    soa_a2_sfgaex2_dims[1] = lat_dim;
    soa_a2_sfgaex2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_a2_sfgaex2", NC_DOUBLE, RANK_soa_a2_sfgaex2, soa_a2_sfgaex2_dims, &soa_a2_sfgaex2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_sfgaex2_id, NC_CHUNKED, soa_a2_sfgaex2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c1_dims[0] = time_dim;
    soa_c1_dims[1] = lev_dim;
    soa_c1_dims[2] = lat_dim;
    soa_c1_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c1", NC_DOUBLE, RANK_soa_c1, soa_c1_dims, &soa_c1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1_id, NC_CHUNKED, soa_c1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c1DDF_dims[0] = time_dim;
    soa_c1DDF_dims[1] = lat_dim;
    soa_c1DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c1DDF", NC_DOUBLE, RANK_soa_c1DDF, soa_c1DDF_dims, &soa_c1DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1DDF_id, NC_CHUNKED, soa_c1DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c1GVF_dims[0] = time_dim;
    soa_c1GVF_dims[1] = lat_dim;
    soa_c1GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c1GVF", NC_DOUBLE, RANK_soa_c1GVF, soa_c1GVF_dims, &soa_c1GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1GVF_id, NC_CHUNKED, soa_c1GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c1SFSBC_dims[0] = time_dim;
    soa_c1SFSBC_dims[1] = lat_dim;
    soa_c1SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c1SFSBC", NC_DOUBLE, RANK_soa_c1SFSBC, soa_c1SFSBC_dims, &soa_c1SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1SFSBC_id, NC_CHUNKED, soa_c1SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c1SFSBS_dims[0] = time_dim;
    soa_c1SFSBS_dims[1] = lat_dim;
    soa_c1SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c1SFSBS", NC_DOUBLE, RANK_soa_c1SFSBS, soa_c1SFSBS_dims, &soa_c1SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1SFSBS_id, NC_CHUNKED, soa_c1SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c1SFSIC_dims[0] = time_dim;
    soa_c1SFSIC_dims[1] = lat_dim;
    soa_c1SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c1SFSIC", NC_DOUBLE, RANK_soa_c1SFSIC, soa_c1SFSIC_dims, &soa_c1SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1SFSIC_id, NC_CHUNKED, soa_c1SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c1SFSIS_dims[0] = time_dim;
    soa_c1SFSIS_dims[1] = lat_dim;
    soa_c1SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c1SFSIS", NC_DOUBLE, RANK_soa_c1SFSIS, soa_c1SFSIS_dims, &soa_c1SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1SFSIS_id, NC_CHUNKED, soa_c1SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c1SFWET_dims[0] = time_dim;
    soa_c1SFWET_dims[1] = lat_dim;
    soa_c1SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c1SFWET", NC_DOUBLE, RANK_soa_c1SFWET, soa_c1SFWET_dims, &soa_c1SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1SFWET_id, NC_CHUNKED, soa_c1SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c1TBF_dims[0] = time_dim;
    soa_c1TBF_dims[1] = lat_dim;
    soa_c1TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c1TBF", NC_DOUBLE, RANK_soa_c1TBF, soa_c1TBF_dims, &soa_c1TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1TBF_id, NC_CHUNKED, soa_c1TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c1_sfcsiz3_dims[0] = time_dim;
    soa_c1_sfcsiz3_dims[1] = lat_dim;
    soa_c1_sfcsiz3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c1_sfcsiz3", NC_DOUBLE, RANK_soa_c1_sfcsiz3, soa_c1_sfcsiz3_dims, &soa_c1_sfcsiz3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1_sfcsiz3_id, NC_CHUNKED, soa_c1_sfcsiz3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c1_sfcsiz4_dims[0] = time_dim;
    soa_c1_sfcsiz4_dims[1] = lat_dim;
    soa_c1_sfcsiz4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c1_sfcsiz4", NC_DOUBLE, RANK_soa_c1_sfcsiz4, soa_c1_sfcsiz4_dims, &soa_c1_sfcsiz4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1_sfcsiz4_id, NC_CHUNKED, soa_c1_sfcsiz4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c1_sfgaex2_dims[0] = time_dim;
    soa_c1_sfgaex2_dims[1] = lat_dim;
    soa_c1_sfgaex2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c1_sfgaex2", NC_DOUBLE, RANK_soa_c1_sfgaex2, soa_c1_sfgaex2_dims, &soa_c1_sfgaex2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1_sfgaex2_id, NC_CHUNKED, soa_c1_sfgaex2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c2_dims[0] = time_dim;
    soa_c2_dims[1] = lev_dim;
    soa_c2_dims[2] = lat_dim;
    soa_c2_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c2", NC_DOUBLE, RANK_soa_c2, soa_c2_dims, &soa_c2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2_id, NC_CHUNKED, soa_c2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c2DDF_dims[0] = time_dim;
    soa_c2DDF_dims[1] = lat_dim;
    soa_c2DDF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c2DDF", NC_DOUBLE, RANK_soa_c2DDF, soa_c2DDF_dims, &soa_c2DDF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2DDF_id, NC_CHUNKED, soa_c2DDF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c2GVF_dims[0] = time_dim;
    soa_c2GVF_dims[1] = lat_dim;
    soa_c2GVF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c2GVF", NC_DOUBLE, RANK_soa_c2GVF, soa_c2GVF_dims, &soa_c2GVF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2GVF_id, NC_CHUNKED, soa_c2GVF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c2SFSBC_dims[0] = time_dim;
    soa_c2SFSBC_dims[1] = lat_dim;
    soa_c2SFSBC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c2SFSBC", NC_DOUBLE, RANK_soa_c2SFSBC, soa_c2SFSBC_dims, &soa_c2SFSBC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2SFSBC_id, NC_CHUNKED, soa_c2SFSBC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c2SFSBS_dims[0] = time_dim;
    soa_c2SFSBS_dims[1] = lat_dim;
    soa_c2SFSBS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c2SFSBS", NC_DOUBLE, RANK_soa_c2SFSBS, soa_c2SFSBS_dims, &soa_c2SFSBS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2SFSBS_id, NC_CHUNKED, soa_c2SFSBS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c2SFSIC_dims[0] = time_dim;
    soa_c2SFSIC_dims[1] = lat_dim;
    soa_c2SFSIC_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c2SFSIC", NC_DOUBLE, RANK_soa_c2SFSIC, soa_c2SFSIC_dims, &soa_c2SFSIC_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2SFSIC_id, NC_CHUNKED, soa_c2SFSIC_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c2SFSIS_dims[0] = time_dim;
    soa_c2SFSIS_dims[1] = lat_dim;
    soa_c2SFSIS_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c2SFSIS", NC_DOUBLE, RANK_soa_c2SFSIS, soa_c2SFSIS_dims, &soa_c2SFSIS_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2SFSIS_id, NC_CHUNKED, soa_c2SFSIS_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c2SFWET_dims[0] = time_dim;
    soa_c2SFWET_dims[1] = lat_dim;
    soa_c2SFWET_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c2SFWET", NC_DOUBLE, RANK_soa_c2SFWET, soa_c2SFWET_dims, &soa_c2SFWET_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2SFWET_id, NC_CHUNKED, soa_c2SFWET_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c2TBF_dims[0] = time_dim;
    soa_c2TBF_dims[1] = lat_dim;
    soa_c2TBF_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c2TBF", NC_DOUBLE, RANK_soa_c2TBF, soa_c2TBF_dims, &soa_c2TBF_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2TBF_id, NC_CHUNKED, soa_c2TBF_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c2_sfcsiz3_dims[0] = time_dim;
    soa_c2_sfcsiz3_dims[1] = lat_dim;
    soa_c2_sfcsiz3_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c2_sfcsiz3", NC_DOUBLE, RANK_soa_c2_sfcsiz3, soa_c2_sfcsiz3_dims, &soa_c2_sfcsiz3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2_sfcsiz3_id, NC_CHUNKED, soa_c2_sfcsiz3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c2_sfcsiz4_dims[0] = time_dim;
    soa_c2_sfcsiz4_dims[1] = lat_dim;
    soa_c2_sfcsiz4_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c2_sfcsiz4", NC_DOUBLE, RANK_soa_c2_sfcsiz4, soa_c2_sfcsiz4_dims, &soa_c2_sfcsiz4_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2_sfcsiz4_id, NC_CHUNKED, soa_c2_sfcsiz4_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    soa_c2_sfgaex2_dims[0] = time_dim;
    soa_c2_sfgaex2_dims[1] = lat_dim;
    soa_c2_sfgaex2_dims[2] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "soa_c2_sfgaex2", NC_DOUBLE, RANK_soa_c2_sfgaex2, soa_c2_sfgaex2_dims, &soa_c2_sfgaex2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2_sfgaex2_id, NC_CHUNKED, soa_c2_sfgaex2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    wat_a1_dims[0] = time_dim;
    wat_a1_dims[1] = lev_dim;
    wat_a1_dims[2] = lat_dim;
    wat_a1_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "wat_a1", NC_DOUBLE, RANK_wat_a1, wat_a1_dims, &wat_a1_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, wat_a1_id, NC_CHUNKED, wat_a1_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    wat_a2_dims[0] = time_dim;
    wat_a2_dims[1] = lev_dim;
    wat_a2_dims[2] = lat_dim;
    wat_a2_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "wat_a2", NC_DOUBLE, RANK_wat_a2, wat_a2_dims, &wat_a2_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, wat_a2_id, NC_CHUNKED, wat_a2_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    wat_a3_dims[0] = time_dim;
    wat_a3_dims[1] = lev_dim;
    wat_a3_dims[2] = lat_dim;
    wat_a3_dims[3] = lon_dim;
    stat = nc_def_var(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, "wat_a3", NC_DOUBLE, RANK_wat_a3, wat_a3_dims, &wat_a3_id);
    check_err(stat,__LINE__,__FILE__);
    stat = nc_def_var_chunking(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, wat_a3_id, NC_CHUNKED, wat_a3_chunksizes);
    check_err(stat,__LINE__,__FILE__);

    /* assign global attributes */

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NC_GLOBAL, "Conventions", 6, "CF-1.0");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NC_GLOBAL, "source", 3, "CAM");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NC_GLOBAL, "case", 6, "camrun");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NC_GLOBAL, "title", 1, "");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NC_GLOBAL, "logname", 8, "jedwards");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NC_GLOBAL, "host", 1, "");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NC_GLOBAL, "Version", 9, "$Name:  $");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NC_GLOBAL, "revision_Id", 54, "$Id: ref_camrun.cdl,v 1.1 2010/05/17 14:43:11 ed Exp $");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NC_GLOBAL, "initial_file", 81, "/fs/cgd/csm/inputdata/atm/cam/inic/fv/cami-mam3_0000-01-01_1.9x2.5_L30_c090306.nc");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NC_GLOBAL, "topography_file", 72, "/fs/cgd/csm/inputdata/atm/cam/topo/USGS-gtopo30_1.9x2.5_remap_c050602.nc");
    check_err(stat,__LINE__,__FILE__);
    }


    /* assign per-variable attributes */

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, P0_id, "long_name", 18, "reference pressure");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, P0_id, "units", 2, "Pa");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, lat_id, "long_name", 8, "latitude");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, lat_id, "units", 13, "degrees_north");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, lon_id, "long_name", 9, "longitude");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, lon_id, "units", 12, "degrees_east");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, slat_id, "long_name", 18, "staggered latitude");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, slat_id, "units", 13, "degrees_north");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, slon_id, "long_name", 19, "staggered longitude");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, slon_id, "units", 12, "degrees_east");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, w_stag_id, "long_name", 26, "staggered latitude weights");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, lev_id, "long_name", 38, "hybrid level at midpoints (1000*(A+B))");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, lev_id, "units", 5, "level");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, lev_id, "positive", 4, "down");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, lev_id, "standard_name", 43, "atmosphere_hybrid_sigma_pressure_coordinate");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, lev_id, "formula_terms", 29, "a: hyam b: hybm p0: P0 ps: PS");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ilev_id, "long_name", 39, "hybrid level at interfaces (1000*(A+B))");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ilev_id, "units", 5, "level");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ilev_id, "positive", 4, "down");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ilev_id, "standard_name", 43, "atmosphere_hybrid_sigma_pressure_coordinate");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ilev_id, "formula_terms", 29, "a: hyai b: hybi p0: P0 ps: PS");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, isccp_prs_id, "long_name", 19, "Mean ISCCP pressure");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, isccp_prs_id, "units", 2, "mb");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double isccp_prs_bnds_att[8] = {((double)0), ((double)180), ((double)310), ((double)440), ((double)560), ((double)680), ((double)800), ((double)1000)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, isccp_prs_id, "isccp_prs_bnds", NC_DOUBLE, 8, isccp_prs_bnds_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, isccp_tau_id, "long_name", 24, "Mean ISCCP optical depth");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, isccp_tau_id, "units", 8, "unitless");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double isccp_tau_bnds_att[8] = {((double)0), ((double)0.3), ((double)1.3), ((double)3.6), ((double)9.4), ((double)23), ((double)60), ((double)379)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, isccp_tau_id, "isccp_tau_bnds", NC_DOUBLE, 8, isccp_tau_bnds_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, isccp_prstau_id, "long_name", 53, "Mean pressure (mb).mean optical depth (unitless)/1000");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, isccp_prstau_id, "units", 5, "mixed");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, time_id, "long_name", 4, "time");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, time_id, "units", 30, "days since 0000-01-01 00:00:00");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, time_id, "calendar", 6, "noleap");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, time_id, "bounds", 9, "time_bnds");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, time_bnds_id, "long_name", 23, "time interval endpoints");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ntrm_id, "long_name", 31, "spectral truncation parameter M");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ntrn_id, "long_name", 31, "spectral truncation parameter N");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ntrk_id, "long_name", 31, "spectral truncation parameter K");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ndbase_id, "long_name", 8, "base day");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, nsbase_id, "long_name", 19, "seconds of base day");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, nbdate_id, "long_name", 20, "base date (YYYYMMDD)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, nbsec_id, "long_name", 20, "seconds of base date");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, mdt_id, "long_name", 8, "timestep");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, mdt_id, "units", 1, "s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, nlon_id, "long_name", 20, "number of longitudes");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, wnummax_id, "long_name", 25, "cutoff Fourier wavenumber");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, hyai_id, "long_name", 40, "hybrid A coefficient at layer interfaces");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, hybi_id, "long_name", 40, "hybrid B coefficient at layer interfaces");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, hyam_id, "long_name", 39, "hybrid A coefficient at layer midpoints");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, hybm_id, "long_name", 39, "hybrid B coefficient at layer midpoints");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, gw_id, "long_name", 13, "gauss weights");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ndcur_id, "long_name", 27, "current day (from base day)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, nscur_id, "long_name", 30, "current seconds of current day");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, date_id, "long_name", 23, "current date (YYYYMMDD)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, co2vmr_id, "long_name", 23, "co2 volume mixing ratio");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ch4vmr_id, "long_name", 23, "ch4 volume mixing ratio");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, n2ovmr_id, "long_name", 23, "n2o volume mixing ratio");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, f11vmr_id, "long_name", 23, "f11 volume mixing ratio");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, f12vmr_id, "long_name", 23, "f12 volume mixing ratio");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, sol_tsi_id, "long_name", 22, "total solar irradiance");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, sol_tsi_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, datesec_id, "long_name", 31, "current seconds of current date");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, nsteph_id, "long_name", 16, "current timestep");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ABSORB_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ABSORB_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ABSORB_id, "units", 2, "/m");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ABSORB_id, "long_name", 18, "Aerosol absorption");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ABSORB_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AEROD_v_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AEROD_v_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AEROD_v_id, "units", 1, "1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AEROD_v_id, "long_name", 43, "Total Aerosol Optical Depth in visible band");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AEROD_v_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODABS_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODABS_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODABS_id, "units", 1, "");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODABS_id, "long_name", 39, "Aerosol absorption optical depth 550 nm");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODABS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODDUST1_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODDUST1_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODDUST1_id, "units", 1, "");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODDUST1_id, "long_name", 46, "Aerosol optical depth 550 nm model 1 from dust");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODDUST1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODDUST2_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODDUST2_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODDUST2_id, "units", 1, "");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODDUST2_id, "long_name", 46, "Aerosol optical depth 550 nm model 2 from dust");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODDUST2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODDUST3_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODDUST3_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODDUST3_id, "units", 1, "");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODDUST3_id, "long_name", 46, "Aerosol optical depth 550 nm model 3 from dust");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODDUST3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODMODE1_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODMODE1_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODMODE1_id, "units", 1, "");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODMODE1_id, "long_name", 35, "Aerosol optical depth 550 nm mode 1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODMODE1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODMODE2_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODMODE2_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODMODE2_id, "units", 1, "");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODMODE2_id, "long_name", 35, "Aerosol optical depth 550 nm mode 2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODMODE2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODMODE3_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODMODE3_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODMODE3_id, "units", 1, "");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODMODE3_id, "long_name", 35, "Aerosol optical depth 550 nm mode 3");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODMODE3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODVIS_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODVIS_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODVIS_id, "units", 1, "");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODVIS_id, "long_name", 28, "Aerosol optical depth 550 nm");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AODVIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQSO4_H2O2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQSO4_H2O2_id, "long_name", 39, "SO4 aqueous phase chemistry due to H2O2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQSO4_H2O2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQSO4_O3_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQSO4_O3_id, "long_name", 37, "SO4 aqueous phase chemistry due to O3");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQSO4_O3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_DMS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_DMS_id, "long_name", 39, "DMS aqueous chemistry (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_DMS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_H2O2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_H2O2_id, "long_name", 40, "H2O2 aqueous chemistry (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_H2O2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_H2SO4_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_H2SO4_id, "long_name", 41, "H2SO4 aqueous chemistry (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_H2SO4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_SO2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_SO2_id, "long_name", 39, "SO2 aqueous chemistry (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_SO2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_SOAG_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_SOAG_id, "long_name", 40, "SOAG aqueous chemistry (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_SOAG_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_bc_a1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_bc_a1_id, "long_name", 41, "bc_a1 aqueous chemistry (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_bc_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_dst_a1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_dst_a1_id, "long_name", 42, "dst_a1 aqueous chemistry (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_dst_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_dst_a3_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_dst_a3_id, "long_name", 42, "dst_a3 aqueous chemistry (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_dst_a3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_ncl_a1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_ncl_a1_id, "long_name", 42, "ncl_a1 aqueous chemistry (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_ncl_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_ncl_a2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_ncl_a2_id, "long_name", 42, "ncl_a2 aqueous chemistry (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_ncl_a2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_ncl_a3_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_ncl_a3_id, "long_name", 42, "ncl_a3 aqueous chemistry (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_ncl_a3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_num_a1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_num_a1_id, "long_name", 42, "num_a1 aqueous chemistry (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_num_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_num_a2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_num_a2_id, "long_name", 42, "num_a2 aqueous chemistry (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_num_a2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_num_a3_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_num_a3_id, "long_name", 42, "num_a3 aqueous chemistry (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_num_a3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_pom_a1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_pom_a1_id, "long_name", 42, "pom_a1 aqueous chemistry (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_pom_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_so4_a1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_so4_a1_id, "long_name", 42, "so4_a1 aqueous chemistry (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_so4_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_so4_a2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_so4_a2_id, "long_name", 42, "so4_a2 aqueous chemistry (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_so4_a2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_so4_a3_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_so4_a3_id, "long_name", 42, "so4_a3 aqueous chemistry (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_so4_a3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_soa_a1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_soa_a1_id, "long_name", 42, "soa_a1 aqueous chemistry (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_soa_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_soa_a2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_soa_a2_id, "long_name", 42, "soa_a2 aqueous chemistry (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, AQ_soa_a2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, BPROD_id, "units", 5, "M2/S3");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, BPROD_id, "long_name", 19, "Buoyancy Production");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, BPROD_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, BURDEN1_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, BURDEN1_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, BURDEN1_id, "units", 5, "kg/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, BURDEN1_id, "long_name", 21, "Aerosol burden mode 1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, BURDEN1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, BURDEN2_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, BURDEN2_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, BURDEN2_id, "units", 5, "kg/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, BURDEN2_id, "long_name", 21, "Aerosol burden mode 2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, BURDEN2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, BURDEN3_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, BURDEN3_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, BURDEN3_id, "units", 5, "kg/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, BURDEN3_id, "long_name", 21, "Aerosol burden mode 3");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, BURDEN3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN1_id, "units", 5, "#/cm3");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN1_id, "long_name", 28, "CCN concentration at S=0.02%");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN2_id, "units", 5, "#/cm3");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN2_id, "long_name", 28, "CCN concentration at S=0.05%");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN3_id, "units", 5, "#/cm3");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN3_id, "long_name", 27, "CCN concentration at S=0.1%");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN4_id, "units", 5, "#/cm3");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN4_id, "long_name", 27, "CCN concentration at S=0.2%");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN5_id, "units", 5, "#/cm3");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN5_id, "long_name", 27, "CCN concentration at S=0.5%");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN5_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN6_id, "units", 5, "#/cm3");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN6_id, "long_name", 27, "CCN concentration at S=1.0%");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CCN6_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDHGH_id, "units", 8, "fraction");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDHGH_id, "long_name", 32, "Vertically-integrated high cloud");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDHGH_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDICE_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDICE_id, "long_name", 34, "Grid box averaged cloud ice amount");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDICE_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDLIQ_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDLIQ_id, "long_name", 37, "Grid box averaged cloud liquid amount");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDLIQ_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDLOW_id, "units", 8, "fraction");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDLOW_id, "long_name", 31, "Vertically-integrated low cloud");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDLOW_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDMED_id, "units", 8, "fraction");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDMED_id, "long_name", 37, "Vertically-integrated mid-level cloud");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDMED_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDTOT_id, "units", 8, "fraction");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDTOT_id, "long_name", 33, "Vertically-integrated total cloud");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLDTOT_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLOUD_id, "units", 8, "fraction");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLOUD_id, "long_name", 14, "Cloud fraction");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CLOUD_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CMFDQ_id, "units", 7, "kg/kg/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CMFDQ_id, "long_name", 32, "QV tendency - shallow convection");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CMFDQ_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CMFDQR_id, "units", 7, "kg/kg/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CMFDQR_id, "long_name", 39, "Q tendency - shallow convection rainout");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CMFDQR_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CMFDT_id, "units", 3, "K/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CMFDT_id, "long_name", 31, "T tendency - shallow convection");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CMFDT_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CMFMC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CMFMC_id, "long_name", 34, "Moist shallow convection mass flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CMFMC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CMFMCDZM_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CMFMCDZM_id, "long_name", 33, "Convection mass flux from ZM deep");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CMFMCDZM_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CONCLD_id, "units", 8, "fraction");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CONCLD_id, "long_name", 22, "Convective cloud cover");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, CONCLD_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DCQ_id, "units", 7, "kg/kg/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DCQ_id, "long_name", 33, "Q tendency due to moist processes");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DCQ_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DMS_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DMS_id, "long_name", 3, "DMS");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DMS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DSTODXC_id, "units", 3, "Tau");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DSTODXC_id, "long_name", 29, "Optical depth for diagnostics");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DSTODXC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DSTSFDRY_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DSTSFDRY_id, "long_name", 30, "Dry deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DSTSFDRY_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DSTSFMBL_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DSTSFMBL_id, "long_name", 28, "Mobilization flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DSTSFMBL_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DSTSFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DSTSFWET_id, "long_name", 30, "Wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DSTSFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DTCOND_id, "units", 3, "K/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DTCOND_id, "long_name", 28, "T tendency - moist processes");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DTCOND_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DTV_id, "units", 3, "K/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DTV_id, "long_name", 20, "T vertical diffusion");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, DTV_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, EXTINCT_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, EXTINCT_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, EXTINCT_id, "units", 2, "/m");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, EXTINCT_id, "long_name", 18, "Aerosol extinction");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, EXTINCT_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FICE_id, "units", 8, "fraction");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FICE_id, "long_name", 35, "Fractional ice content within cloud");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FICE_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLDS_id, "Sampling_Sequence", 8, "rad_lwsw");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLDS_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLDS_id, "long_name", 36, "Downwelling longwave flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLDS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLNS_id, "Sampling_Sequence", 8, "rad_lwsw");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLNS_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLNS_id, "long_name", 28, "Net longwave flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLNS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLNSC_id, "Sampling_Sequence", 8, "rad_lwsw");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLNSC_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLNSC_id, "long_name", 37, "Clearsky net longwave flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLNSC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLNT_id, "Sampling_Sequence", 8, "rad_lwsw");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLNT_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLNT_id, "long_name", 33, "Net longwave flux at top of model");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLNT_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLNTC_id, "Sampling_Sequence", 8, "rad_lwsw");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLNTC_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLNTC_id, "long_name", 42, "Clearsky net longwave flux at top of model");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLNTC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLUT_id, "Sampling_Sequence", 8, "rad_lwsw");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLUT_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLUT_id, "long_name", 39, "Upwelling longwave flux at top of model");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLUT_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLUTC_id, "Sampling_Sequence", 8, "rad_lwsw");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLUTC_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLUTC_id, "long_name", 48, "Clearsky upwelling longwave flux at top of model");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FLUTC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FREQSH_id, "units", 8, "fraction");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FREQSH_id, "long_name", 42, "Fractional occurance of shallow convection");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FREQSH_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FREQZM_id, "units", 8, "fraction");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FREQZM_id, "long_name", 37, "Fractional occurance of ZM convection");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FREQZM_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSDS_id, "Sampling_Sequence", 8, "rad_lwsw");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSDS_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSDS_id, "long_name", 33, "Downwelling solar flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSDS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSDSC_id, "Sampling_Sequence", 8, "rad_lwsw");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSDSC_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSDSC_id, "long_name", 42, "Clearsky downwelling solar flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSDSC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNS_id, "Sampling_Sequence", 8, "rad_lwsw");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNS_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNS_id, "long_name", 25, "Net solar flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNSC_id, "Sampling_Sequence", 8, "rad_lwsw");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNSC_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNSC_id, "long_name", 34, "Clearsky net solar flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNSC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNT_id, "Sampling_Sequence", 8, "rad_lwsw");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNT_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNT_id, "long_name", 30, "Net solar flux at top of model");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNT_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNTC_id, "Sampling_Sequence", 8, "rad_lwsw");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNTC_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNTC_id, "long_name", 39, "Clearsky net solar flux at top of model");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNTC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNTOA_id, "Sampling_Sequence", 8, "rad_lwsw");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNTOA_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNTOA_id, "long_name", 35, "Net solar flux at top of atmosphere");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNTOA_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNTOAC_id, "Sampling_Sequence", 8, "rad_lwsw");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNTOAC_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNTOAC_id, "long_name", 44, "Clearsky net solar flux at top of atmosphere");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSNTOAC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSUTOA_id, "Sampling_Sequence", 8, "rad_lwsw");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSUTOA_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSUTOA_id, "long_name", 41, "Upwelling solar flux at top of atmosphere");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, FSUTOA_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_DMS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_DMS_id, "long_name", 47, "DMS gas chemistry/wet removal (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_DMS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_H2O2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_H2O2_id, "long_name", 48, "H2O2 gas chemistry/wet removal (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_H2O2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_H2SO4_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_H2SO4_id, "long_name", 49, "H2SO4 gas chemistry/wet removal (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_H2SO4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_SO2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_SO2_id, "long_name", 47, "SO2 gas chemistry/wet removal (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_SO2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_SOAG_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_SOAG_id, "long_name", 48, "SOAG gas chemistry/wet removal (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_SOAG_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_bc_a1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_bc_a1_id, "long_name", 49, "bc_a1 gas chemistry/wet removal (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_bc_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_dst_a1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_dst_a1_id, "long_name", 50, "dst_a1 gas chemistry/wet removal (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_dst_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_dst_a3_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_dst_a3_id, "long_name", 50, "dst_a3 gas chemistry/wet removal (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_dst_a3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_ncl_a1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_ncl_a1_id, "long_name", 50, "ncl_a1 gas chemistry/wet removal (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_ncl_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_ncl_a2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_ncl_a2_id, "long_name", 50, "ncl_a2 gas chemistry/wet removal (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_ncl_a2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_ncl_a3_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_ncl_a3_id, "long_name", 50, "ncl_a3 gas chemistry/wet removal (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_ncl_a3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_num_a1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_num_a1_id, "long_name", 50, "num_a1 gas chemistry/wet removal (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_num_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_num_a2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_num_a2_id, "long_name", 50, "num_a2 gas chemistry/wet removal (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_num_a2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_num_a3_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_num_a3_id, "long_name", 50, "num_a3 gas chemistry/wet removal (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_num_a3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_pom_a1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_pom_a1_id, "long_name", 50, "pom_a1 gas chemistry/wet removal (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_pom_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_so4_a1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_so4_a1_id, "long_name", 50, "so4_a1 gas chemistry/wet removal (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_so4_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_so4_a2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_so4_a2_id, "long_name", 50, "so4_a2 gas chemistry/wet removal (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_so4_a2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_so4_a3_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_so4_a3_id, "long_name", 50, "so4_a3 gas chemistry/wet removal (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_so4_a3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_soa_a1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_soa_a1_id, "long_name", 50, "soa_a1 gas chemistry/wet removal (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_soa_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_soa_a2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_soa_a2_id, "long_name", 50, "soa_a2 gas chemistry/wet removal (for gas species)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, GS_soa_a2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, H2O2_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, H2O2_id, "long_name", 4, "H2O2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, H2O2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, H2SO4_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, H2SO4_id, "long_name", 5, "H2SO4");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, H2SO4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, H2SO4_sfgaex1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, H2SO4_sfgaex1_id, "long_name", 50, "H2SO4 gas-aerosol-exchange primary column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, H2SO4_sfgaex1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, H2SO4_sfnnuc1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, H2SO4_sfnnuc1_id, "long_name", 56, "H2SO4 modal_aero new particle nucleation column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, H2SO4_sfnnuc1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ICEFRAC_id, "units", 8, "fraction");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ICEFRAC_id, "long_name", 39, "Fraction of sfc area covered by sea-ice");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ICEFRAC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ICIMR_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ICIMR_id, "long_name", 36, "Prognostic in-cloud ice mixing ratio");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ICIMR_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ICWMR_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ICWMR_id, "long_name", 38, "Prognostic in-cloud water mixing ratio");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ICWMR_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, KVH_id, "units", 4, "m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, KVH_id, "long_name", 48, "Vertical diffusion diffusivities (heat/moisture)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, KVH_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, KVM_id, "units", 4, "m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, KVM_id, "long_name", 43, "Vertical diffusion diffusivities (momentum)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, KVM_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, LANDFRAC_id, "units", 8, "fraction");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, LANDFRAC_id, "long_name", 36, "Fraction of sfc area covered by land");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, LANDFRAC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, LCLOUD_id, "units", 1, "");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, LCLOUD_id, "long_name", 21, "Liquid cloud fraction");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, LCLOUD_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, LHFLX_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, LHFLX_id, "long_name", 24, "Surface latent heat flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, LHFLX_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, LND_MBL_id, "units", 4, "frac");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, LND_MBL_id, "long_name", 23, "Soil erodibility factor");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, LND_MBL_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, LWCF_id, "Sampling_Sequence", 8, "rad_lwsw");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, LWCF_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, LWCF_id, "long_name", 22, "Longwave cloud forcing");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, LWCF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NDROPCOL_id, "units", 4, "#/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NDROPCOL_id, "long_name", 21, "Column droplet number");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NDROPCOL_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NDROPMIX_id, "units", 6, "#/kg/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NDROPMIX_id, "long_name", 21, "Droplet number mixing");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NDROPMIX_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NDROPSNK_id, "units", 6, "#/kg/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NDROPSNK_id, "long_name", 35, "Droplet number loss by microphysics");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NDROPSNK_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NDROPSRC_id, "units", 6, "#/kg/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NDROPSRC_id, "long_name", 21, "Droplet number source");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NDROPSRC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NUMICE_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NUMICE_id, "long_name", 34, "Grid box averaged cloud ice number");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NUMICE_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NUMLIQ_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NUMLIQ_id, "long_name", 37, "Grid box averaged cloud liquid number");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, NUMLIQ_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, OCNFRAC_id, "units", 8, "fraction");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, OCNFRAC_id, "long_name", 37, "Fraction of sfc area covered by ocean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, OCNFRAC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_bc_a1_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_bc_a1_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_bc_a1_id, "units", 1, "1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_bc_a1_id, "long_name", 35, "bc_a1 optical depth in visible band");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_bc_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_dst_a1_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_dst_a1_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_dst_a1_id, "units", 1, "1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_dst_a1_id, "long_name", 36, "dst_a1 optical depth in visible band");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_dst_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_dst_a3_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_dst_a3_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_dst_a3_id, "units", 1, "1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_dst_a3_id, "long_name", 36, "dst_a3 optical depth in visible band");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_dst_a3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_ncl_a1_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_ncl_a1_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_ncl_a1_id, "units", 1, "1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_ncl_a1_id, "long_name", 36, "ncl_a1 optical depth in visible band");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_ncl_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_ncl_a3_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_ncl_a3_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_ncl_a3_id, "units", 1, "1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_ncl_a3_id, "long_name", 36, "ncl_a3 optical depth in visible band");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_ncl_a3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_pom_a1_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_pom_a1_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_pom_a1_id, "units", 1, "1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_pom_a1_id, "long_name", 36, "pom_a1 optical depth in visible band");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_pom_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_so4_a1_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_so4_a1_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_so4_a1_id, "units", 1, "1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_so4_a1_id, "long_name", 36, "so4_a1 optical depth in visible band");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_so4_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_soa_a1_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_soa_a1_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_soa_a1_id, "units", 1, "1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_soa_a1_id, "long_name", 36, "soa_a1 optical depth in visible band");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ODV_soa_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, OMEGA_id, "units", 4, "Pa/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, OMEGA_id, "long_name", 28, "Vertical velocity (pressure)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, OMEGA_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, OMEGAT_id, "units", 6, "K Pa/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, OMEGAT_id, "long_name", 18, "Vertical heat flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, OMEGAT_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ORO_id, "units", 4, "frac");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ORO_id, "long_name", 3, "ORO");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ORO_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PBLH_id, "units", 1, "m");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PBLH_id, "long_name", 10, "PBL height");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PBLH_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PCONVB_id, "units", 2, "Pa");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PCONVB_id, "long_name", 24, "convection base pressure");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PCONVB_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PCONVT_id, "units", 2, "Pa");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PCONVT_id, "long_name", 24, "convection top  pressure");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PCONVT_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PHIS_id, "units", 5, "m2/s2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PHIS_id, "long_name", 20, "Surface geopotential");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECC_id, "units", 3, "m/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECC_id, "long_name", 41, "Convective precipitation rate (liq + ice)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECCDZM_id, "units", 3, "m/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECCDZM_id, "long_name", 42, "Convective precipitation rate from ZM deep");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECCDZM_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECL_id, "units", 3, "m/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECL_id, "long_name", 51, "Large-scale (stable) precipitation rate (liq + ice)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECL_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECSC_id, "units", 3, "m/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECSC_id, "long_name", 39, "Convective snow rate (water equivalent)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECSC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECSH_id, "units", 3, "m/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECSH_id, "long_name", 37, "Shallow Convection precipitation rate");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECSH_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECSL_id, "units", 3, "m/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECSL_id, "long_name", 49, "Large-scale (stable) snow rate (water equivalent)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECSL_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECT_id, "units", 3, "m/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECT_id, "long_name", 65, "Total (convective and large-scale) precipitation rate (liq + ice)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PRECT_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PS_id, "units", 2, "Pa");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PS_id, "long_name", 16, "Surface pressure");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PSL_id, "units", 2, "Pa");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PSL_id, "long_name", 18, "Sea level pressure");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, PSL_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, Q_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, Q_id, "long_name", 17, "Specific humidity");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, Q_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QC_id, "units", 7, "kg/kg/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QC_id, "long_name", 41, "Q tendency - shallow convection LW export");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QFLX_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QFLX_id, "long_name", 18, "Surface water flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QFLX_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QREFHT_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QREFHT_id, "long_name", 25, "Reference height humidity");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QREFHT_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QRL_id, "Sampling_Sequence", 8, "rad_lwsw");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QRL_id, "units", 3, "K/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QRL_id, "long_name", 21, "Longwave heating rate");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QRL_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QRS_id, "Sampling_Sequence", 8, "rad_lwsw");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QRS_id, "units", 3, "K/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QRS_id, "long_name", 18, "Solar heating rate");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QRS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QT_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QT_id, "long_name", 24, "Total water mixing ratio");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QT_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QTFLX_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QTFLX_id, "long_name", 16, "Total water flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, QTFLX_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, RAM1_id, "units", 4, "frac");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, RAM1_id, "long_name", 4, "RAM1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, RAM1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, RELHUM_id, "units", 7, "percent");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, RELHUM_id, "long_name", 17, "Relative humidity");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, RELHUM_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, RHREFHT_id, "units", 8, "fraction");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, RHREFHT_id, "long_name", 34, "Reference height relative humidity");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, RHREFHT_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SFCLDICE_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SFCLDICE_id, "long_name", 19, "CLDICE surface flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SFCLDICE_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SFCLDLIQ_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SFCLDLIQ_id, "long_name", 19, "CLDLIQ surface flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SFCLDLIQ_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SFI_id, "units", 8, "FRACTION");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SFI_id, "long_name", 24, "Interface-layer sat frac");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SFI_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SFNUMICE_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SFNUMICE_id, "long_name", 19, "NUMICE surface flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SFNUMICE_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SFNUMLIQ_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SFNUMLIQ_id, "long_name", 19, "NUMLIQ surface flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SFNUMLIQ_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SHFLX_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SHFLX_id, "long_name", 26, "Surface sensible heat flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SHFLX_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SL_id, "units", 4, "J/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SL_id, "long_name", 26, "Liquid water static energy");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SL_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SLFLX_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SLFLX_id, "long_name", 25, "Liquid static energy flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SLFLX_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SLV_id, "units", 4, "J/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SLV_id, "long_name", 29, "Liq wat virtual static energy");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SLV_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SNOWHICE_id, "units", 1, "m");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SNOWHICE_id, "long_name", 27, "Water equivalent snow depth");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SNOWHICE_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SNOWHLND_id, "units", 1, "m");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SNOWHLND_id, "long_name", 27, "Water equivalent snow depth");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SNOWHLND_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SO2_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SO2_id, "long_name", 3, "SO2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SO2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SO2_CLXF_id, "units", 11, "molec/cm2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SO2_CLXF_id, "long_name", 47, "vertically intergrated external forcing for SO2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SO2_CLXF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SO2_XFRC_id, "units", 11, "molec/cm3/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SO2_XFRC_id, "long_name", 24, "external forcing for SO2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SO2_XFRC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SOAG_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SOAG_id, "long_name", 4, "SOAG");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SOAG_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SOAG_sfgaex1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SOAG_sfgaex1_id, "long_name", 49, "SOAG gas-aerosol-exchange primary column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SOAG_sfgaex1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SOLIN_id, "Sampling_Sequence", 8, "rad_lwsw");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SOLIN_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SOLIN_id, "long_name", 16, "Solar insolation");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SOLIN_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SPROD_id, "units", 5, "M2/S3");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SPROD_id, "long_name", 16, "Shear Production");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SPROD_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SRFRAD_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SRFRAD_id, "long_name", 29, "Net radiative flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SRFRAD_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SSAVIS_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SSAVIS_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SSAVIS_id, "units", 1, "");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SSAVIS_id, "long_name", 29, "Aerosol singel-scatter albedo");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SSAVIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SSTODXC_id, "units", 3, "Tau");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SSTODXC_id, "long_name", 29, "Optical depth for diagnostics");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SSTODXC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SSTSFDRY_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SSTSFDRY_id, "long_name", 30, "Dry deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SSTSFDRY_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SSTSFMBL_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SSTSFMBL_id, "long_name", 28, "Mobilization flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SSTSFMBL_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SSTSFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SSTSFWET_id, "long_name", 30, "Wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SSTSFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SWCF_id, "Sampling_Sequence", 8, "rad_lwsw");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SWCF_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SWCF_id, "long_name", 23, "Shortwave cloud forcing");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, SWCF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, T_id, "units", 1, "K");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, T_id, "long_name", 11, "Temperature");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, T_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TAUTMSX_id, "units", 4, "N/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TAUTMSX_id, "long_name", 44, "Zonal      turbulent mountain surface stress");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TAUTMSX_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TAUTMSY_id, "units", 4, "N/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TAUTMSY_id, "long_name", 44, "Meridional turbulent mountain surface stress");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TAUTMSY_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TAUX_id, "units", 4, "N/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TAUX_id, "long_name", 20, "Zonal surface stress");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TAUX_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TAUY_id, "units", 4, "N/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TAUY_id, "long_name", 25, "Meridional surface stress");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TAUY_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TGCLDCWP_id, "units", 5, "kg/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TGCLDCWP_id, "long_name", 48, "Total grid-box cloud water path (liquid and ice)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TGCLDCWP_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TGCLDIWP_id, "units", 5, "kg/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TGCLDIWP_id, "long_name", 35, "Total grid-box cloud ice water path");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TGCLDIWP_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TGCLDLWP_id, "units", 5, "kg/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TGCLDLWP_id, "long_name", 38, "Total grid-box cloud liquid water path");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TGCLDLWP_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TKE_id, "units", 5, "m2/s2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TKE_id, "long_name", 24, "Turbulent Kinetic Energy");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TKE_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TMQ_id, "units", 5, "kg/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TMQ_id, "long_name", 50, "Total (vertically integrated) precipitatable water");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TMQ_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TREFHT_id, "units", 1, "K");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TREFHT_id, "long_name", 28, "Reference height temperature");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TREFHT_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TREFMNAV_id, "units", 1, "K");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TREFMNAV_id, "long_name", 31, "Average of TREFHT daily minimum");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TREFMNAV_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TREFMXAV_id, "units", 1, "K");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TREFMXAV_id, "long_name", 31, "Average of TREFHT daily maximum");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TREFMXAV_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_FD_id, "units", 11, "probability");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_FD_id, "long_name", 16, "Tropopause Found");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_FD_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_P_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_P_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_P_id, "units", 2, "Pa");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_P_id, "long_name", 19, "Tropopause Pressure");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_P_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_PD_id, "units", 11, "probability");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_PD_id, "long_name", 21, "Tropopause Probabilty");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_PD_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_T_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_T_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_T_id, "units", 1, "K");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_T_id, "long_name", 22, "Tropopause Temperature");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_T_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double _FillValue_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_Z_id, "_FillValue", NC_DOUBLE, 1, _FillValue_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    static const double missing_value_att[1] = {((double)1e+36)} ;
    stat = nc_put_att_double(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_Z_id, "missing_value", NC_DOUBLE, 1, missing_value_att);
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_Z_id, "units", 1, "m");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_Z_id, "long_name", 17, "Tropopause Height");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TROP_Z_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TS_id, "units", 1, "K");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TS_id, "long_name", 31, "Surface temperature (radiative)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TSMN_id, "units", 1, "K");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TSMN_id, "long_name", 46, "Minimum surface temperature over output period");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TSMN_id, "cell_methods", 13, "time: minimum");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TSMX_id, "units", 1, "K");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TSMX_id, "long_name", 46, "Maximum surface temperature over output period");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, TSMX_id, "cell_methods", 13, "time: maximum");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, U_id, "units", 3, "m/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, U_id, "long_name", 10, "Zonal wind");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, U_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, UFLX_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, UFLX_id, "long_name", 19, "Zonal momentum flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, UFLX_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, US_id, "units", 3, "m/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, US_id, "long_name", 21, "Zonal wind, staggered");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, US_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, UU_id, "units", 5, "m2/s2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, UU_id, "long_name", 22, "Zonal velocity squared");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, UU_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, V_id, "units", 3, "m/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, V_id, "long_name", 15, "Meridional wind");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, V_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VD01_id, "units", 7, "kg/kg/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VD01_id, "long_name", 23, "Vertical diffusion of Q");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VD01_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VFLX_id, "units", 4, "W/m2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VFLX_id, "long_name", 23, "Meridional momentm flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VFLX_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VQ_id, "units", 8, "m/skg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VQ_id, "long_name", 26, "Meridional water transport");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VQ_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VS_id, "units", 3, "m/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VS_id, "long_name", 26, "Meridional wind, staggered");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VT_id, "units", 5, "K m/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VT_id, "long_name", 25, "Meridional heat transport");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VT_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VU_id, "units", 5, "m2/s2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VU_id, "long_name", 33, "Meridional flux of zonal momentum");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VU_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VV_id, "units", 5, "m2/s2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VV_id, "long_name", 27, "Meridional velocity squared");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, VV_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, WGUSTD_id, "units", 3, "m/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, WGUSTD_id, "long_name", 26, "wind gusts from turbulence");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, WGUSTD_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, WTKE_id, "units", 3, "m/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, WTKE_id, "long_name", 38, "Standard deviation of updraft velocity");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, WTKE_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, XPH_LWC_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, XPH_LWC_id, "long_name", 26, "pH value multiplied by lwc");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, XPH_LWC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, Z3_id, "units", 1, "m");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, Z3_id, "long_name", 37, "Geopotential Height (above sea level)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, Z3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, airFV_id, "units", 4, "frac");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, airFV_id, "long_name", 2, "FV");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, airFV_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1_id, "long_name", 5, "bc_a1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1DDF_id, "long_name", 49, "bc_a1 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1GVF_id, "long_name", 39, "bc_a1 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1SFSBC_id, "long_name", 55, "Wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1SFSBS_id, "long_name", 55, "Wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1SFSIC_id, "long_name", 52, "Wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1SFSIS_id, "long_name", 52, "Wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1SFWET_id, "long_name", 30, "Wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1TBF_id, "long_name", 35, "bc_a1 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1_CLXF_id, "units", 11, "molec/cm2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1_CLXF_id, "long_name", 49, "vertically intergrated external forcing for bc_a1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1_CLXF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1_XFRC_id, "units", 11, "molec/cm3/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1_XFRC_id, "long_name", 26, "external forcing for bc_a1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_a1_XFRC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1_id, "long_name", 20, "bc_c1 in cloud water");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1DDF_id, "long_name", 49, "bc_c1 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1GVF_id, "long_name", 39, "bc_c1 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1SFSBC_id, "long_name", 61, "bc_c1 wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1SFSBS_id, "long_name", 61, "bc_c1 wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1SFSIC_id, "long_name", 58, "bc_c1 wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1SFSIS_id, "long_name", 58, "bc_c1 wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1SFWET_id, "long_name", 36, "bc_c1 wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1TBF_id, "long_name", 35, "bc_c1 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, bc_c1TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, chem_trop_id, "units", 22, "fraction of model time");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, chem_trop_id, "long_name", 41, "Lowest level with stratospheric chemsitry");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, chem_trop_tropop_id, "units", 22, "fraction of model time");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, chem_trop_tropop_id, "long_name", 44, "Troposphere boundary calculated in chemistry");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnd_a01_id, "units", 1, "m");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnd_a01_id, "long_name", 32, "dry dgnum, interstitial, mode 01");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnd_a01_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnd_a02_id, "units", 1, "m");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnd_a02_id, "long_name", 32, "dry dgnum, interstitial, mode 02");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnd_a02_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnd_a03_id, "units", 1, "m");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnd_a03_id, "long_name", 32, "dry dgnum, interstitial, mode 03");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnd_a03_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnw_a01_id, "units", 1, "m");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnw_a01_id, "long_name", 32, "wet dgnum, interstitial, mode 01");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnw_a01_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnw_a02_id, "units", 1, "m");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnw_a02_id, "long_name", 32, "wet dgnum, interstitial, mode 02");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnw_a02_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnw_a03_id, "units", 1, "m");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnw_a03_id, "long_name", 32, "wet dgnum, interstitial, mode 03");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dgnw_a03_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1_id, "long_name", 6, "dst_a1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1DDF_id, "long_name", 50, "dst_a1 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1GVF_id, "long_name", 40, "dst_a1 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SF_id, "long_name", 28, "dst_a1 dust surface emission");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SFSBC_id, "long_name", 55, "Wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SFSBS_id, "long_name", 55, "Wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SFSIC_id, "long_name", 52, "Wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SFSIS_id, "long_name", 52, "Wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SFWET_id, "long_name", 30, "Wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1TBF_id, "long_name", 36, "dst_a1 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a1TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3_id, "long_name", 6, "dst_a3");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3DDF_id, "long_name", 50, "dst_a3 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3GVF_id, "long_name", 40, "dst_a3 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SF_id, "long_name", 28, "dst_a3 dust surface emission");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SFSBC_id, "long_name", 55, "Wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SFSBS_id, "long_name", 55, "Wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SFSIC_id, "long_name", 52, "Wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SFSIS_id, "long_name", 52, "Wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SFWET_id, "long_name", 30, "Wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3TBF_id, "long_name", 36, "dst_a3 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_a3TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1_id, "long_name", 21, "dst_c1 in cloud water");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1DDF_id, "long_name", 50, "dst_c1 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1GVF_id, "long_name", 40, "dst_c1 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1SFSBC_id, "long_name", 62, "dst_c1 wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1SFSBS_id, "long_name", 62, "dst_c1 wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1SFSIC_id, "long_name", 59, "dst_c1 wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1SFSIS_id, "long_name", 59, "dst_c1 wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1SFWET_id, "long_name", 37, "dst_c1 wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1TBF_id, "long_name", 36, "dst_c1 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c1TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3_id, "long_name", 21, "dst_c3 in cloud water");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3DDF_id, "long_name", 50, "dst_c3 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3GVF_id, "long_name", 40, "dst_c3 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3SFSBC_id, "long_name", 62, "dst_c3 wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3SFSBS_id, "long_name", 62, "dst_c3 wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3SFSIC_id, "long_name", 59, "dst_c3 wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3SFSIS_id, "long_name", 59, "dst_c3 wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3SFWET_id, "long_name", 37, "dst_c3 wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3TBF_id, "long_name", 36, "dst_c3 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, dst_c3TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1_id, "long_name", 6, "ncl_a1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1DDF_id, "long_name", 50, "ncl_a1 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1GVF_id, "long_name", 40, "ncl_a1 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SF_id, "long_name", 36, "ncl_a1 progseasalts surface emission");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SFSBC_id, "long_name", 55, "Wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SFSBS_id, "long_name", 55, "Wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SFSIC_id, "long_name", 52, "Wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SFSIS_id, "long_name", 52, "Wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SFWET_id, "long_name", 30, "Wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1TBF_id, "long_name", 36, "ncl_a1 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1_sfcoag1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1_sfcoag1_id, "long_name", 45, "ncl_a1 modal_aero coagulation column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1_sfcoag1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1_sfcsiz3_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1_sfcsiz3_id, "long_name", 54, "ncl_a1 calcsize aitken-to-accum adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1_sfcsiz3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1_sfcsiz4_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1_sfcsiz4_id, "long_name", 54, "ncl_a1 calcsize accum-to-aitken adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1_sfcsiz4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1_sfgaex2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1_sfgaex2_id, "long_name", 52, "ncl_a1 gas-aerosol-exchange renaming column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a1_sfgaex2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2_id, "long_name", 6, "ncl_a2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2DDF_id, "long_name", 50, "ncl_a2 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2GVF_id, "long_name", 40, "ncl_a2 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SF_id, "long_name", 36, "ncl_a2 progseasalts surface emission");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SFSBC_id, "long_name", 55, "Wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SFSBS_id, "long_name", 55, "Wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SFSIC_id, "long_name", 52, "Wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SFSIS_id, "long_name", 52, "Wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SFWET_id, "long_name", 30, "Wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2TBF_id, "long_name", 36, "ncl_a2 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2_sfcoag1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2_sfcoag1_id, "long_name", 45, "ncl_a2 modal_aero coagulation column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2_sfcoag1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2_sfcsiz3_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2_sfcsiz3_id, "long_name", 54, "ncl_a2 calcsize aitken-to-accum adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2_sfcsiz3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2_sfcsiz4_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2_sfcsiz4_id, "long_name", 54, "ncl_a2 calcsize accum-to-aitken adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2_sfcsiz4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2_sfgaex2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2_sfgaex2_id, "long_name", 52, "ncl_a2 gas-aerosol-exchange renaming column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a2_sfgaex2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3_id, "long_name", 6, "ncl_a3");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3DDF_id, "long_name", 50, "ncl_a3 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3GVF_id, "long_name", 40, "ncl_a3 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SF_id, "long_name", 36, "ncl_a3 progseasalts surface emission");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SFSBC_id, "long_name", 55, "Wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SFSBS_id, "long_name", 55, "Wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SFSIC_id, "long_name", 52, "Wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SFSIS_id, "long_name", 52, "Wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SFWET_id, "long_name", 30, "Wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3TBF_id, "long_name", 36, "ncl_a3 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_a3TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1_id, "long_name", 21, "ncl_c1 in cloud water");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1DDF_id, "long_name", 50, "ncl_c1 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1GVF_id, "long_name", 40, "ncl_c1 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1SFSBC_id, "long_name", 62, "ncl_c1 wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1SFSBS_id, "long_name", 62, "ncl_c1 wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1SFSIC_id, "long_name", 59, "ncl_c1 wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1SFSIS_id, "long_name", 59, "ncl_c1 wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1SFWET_id, "long_name", 37, "ncl_c1 wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1TBF_id, "long_name", 36, "ncl_c1 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1_sfcsiz3_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1_sfcsiz3_id, "long_name", 54, "ncl_c1 calcsize aitken-to-accum adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1_sfcsiz3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1_sfcsiz4_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1_sfcsiz4_id, "long_name", 54, "ncl_c1 calcsize accum-to-aitken adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1_sfcsiz4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1_sfgaex2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1_sfgaex2_id, "long_name", 52, "ncl_c1 gas-aerosol-exchange renaming column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c1_sfgaex2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2_id, "long_name", 21, "ncl_c2 in cloud water");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2DDF_id, "long_name", 50, "ncl_c2 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2GVF_id, "long_name", 40, "ncl_c2 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2SFSBC_id, "long_name", 62, "ncl_c2 wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2SFSBS_id, "long_name", 62, "ncl_c2 wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2SFSIC_id, "long_name", 59, "ncl_c2 wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2SFSIS_id, "long_name", 59, "ncl_c2 wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2SFWET_id, "long_name", 37, "ncl_c2 wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2TBF_id, "long_name", 36, "ncl_c2 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2_sfcsiz3_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2_sfcsiz3_id, "long_name", 54, "ncl_c2 calcsize aitken-to-accum adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2_sfcsiz3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2_sfcsiz4_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2_sfcsiz4_id, "long_name", 54, "ncl_c2 calcsize accum-to-aitken adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2_sfcsiz4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2_sfgaex2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2_sfgaex2_id, "long_name", 52, "ncl_c2 gas-aerosol-exchange renaming column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c2_sfgaex2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3_id, "long_name", 21, "ncl_c3 in cloud water");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3DDF_id, "long_name", 50, "ncl_c3 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3GVF_id, "long_name", 40, "ncl_c3 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3SFSBC_id, "long_name", 62, "ncl_c3 wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3SFSBS_id, "long_name", 62, "ncl_c3 wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3SFSIC_id, "long_name", 59, "ncl_c3 wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3SFSIS_id, "long_name", 59, "ncl_c3 wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3SFWET_id, "long_name", 37, "ncl_c3 wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3TBF_id, "long_name", 36, "ncl_c3 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, ncl_c3TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_id, "long_name", 6, "num_a1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1DDF_id, "long_name", 50, "num_a1 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1GVF_id, "long_name", 40, "num_a1 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1SFSBC_id, "long_name", 55, "Wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1SFSBS_id, "long_name", 55, "Wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1SFSIC_id, "long_name", 52, "Wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1SFSIS_id, "long_name", 52, "Wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1SFWET_id, "long_name", 30, "Wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1TBF_id, "long_name", 36, "num_a1 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_CLXF_id, "units", 11, "molec/cm2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_CLXF_id, "long_name", 50, "vertically intergrated external forcing for num_a1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_CLXF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_XFRC_id, "units", 11, "molec/cm3/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_XFRC_id, "long_name", 27, "external forcing for num_a1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_XFRC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfcoag1_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfcoag1_id, "long_name", 45, "num_a1 modal_aero coagulation column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfcoag1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfcsiz1_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfcsiz1_id, "long_name", 43, "num_a1 calcsize number-adjust column source");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfcsiz1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfcsiz2_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfcsiz2_id, "long_name", 41, "num_a1 calcsize number-adjust column sink");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfcsiz2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfcsiz3_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfcsiz3_id, "long_name", 54, "num_a1 calcsize aitken-to-accum adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfcsiz3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfcsiz4_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfcsiz4_id, "long_name", 54, "num_a1 calcsize accum-to-aitken adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfcsiz4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfgaex2_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfgaex2_id, "long_name", 52, "num_a1 gas-aerosol-exchange renaming column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a1_sfgaex2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_id, "long_name", 6, "num_a2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2DDF_id, "long_name", 50, "num_a2 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2GVF_id, "long_name", 40, "num_a2 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2SFSBC_id, "long_name", 55, "Wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2SFSBS_id, "long_name", 55, "Wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2SFSIC_id, "long_name", 52, "Wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2SFSIS_id, "long_name", 52, "Wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2SFWET_id, "long_name", 30, "Wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2TBF_id, "long_name", 36, "num_a2 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_CLXF_id, "units", 11, "molec/cm2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_CLXF_id, "long_name", 50, "vertically intergrated external forcing for num_a2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_CLXF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_XFRC_id, "units", 11, "molec/cm3/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_XFRC_id, "long_name", 27, "external forcing for num_a2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_XFRC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfcoag1_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfcoag1_id, "long_name", 45, "num_a2 modal_aero coagulation column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfcoag1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfcsiz1_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfcsiz1_id, "long_name", 43, "num_a2 calcsize number-adjust column source");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfcsiz1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfcsiz2_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfcsiz2_id, "long_name", 41, "num_a2 calcsize number-adjust column sink");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfcsiz2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfcsiz3_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfcsiz3_id, "long_name", 54, "num_a2 calcsize aitken-to-accum adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfcsiz3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfcsiz4_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfcsiz4_id, "long_name", 54, "num_a2 calcsize accum-to-aitken adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfcsiz4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfgaex2_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfgaex2_id, "long_name", 52, "num_a2 gas-aerosol-exchange renaming column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfgaex2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfnnuc1_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfnnuc1_id, "long_name", 57, "num_a2 modal_aero new particle nucleation column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a2_sfnnuc1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3_id, "long_name", 6, "num_a3");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3DDF_id, "long_name", 50, "num_a3 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3GVF_id, "long_name", 40, "num_a3 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3SFSBC_id, "long_name", 55, "Wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3SFSBS_id, "long_name", 55, "Wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3SFSIC_id, "long_name", 52, "Wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3SFSIS_id, "long_name", 52, "Wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3SFWET_id, "long_name", 30, "Wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3TBF_id, "long_name", 36, "num_a3 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3_sfcsiz1_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3_sfcsiz1_id, "long_name", 43, "num_a3 calcsize number-adjust column source");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3_sfcsiz1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3_sfcsiz2_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3_sfcsiz2_id, "long_name", 41, "num_a3 calcsize number-adjust column sink");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_a3_sfcsiz2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_id, "long_name", 21, "num_c1 in cloud water");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1DDF_id, "long_name", 50, "num_c1 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1GVF_id, "long_name", 40, "num_c1 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1SFSBC_id, "long_name", 62, "num_c1 wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1SFSBS_id, "long_name", 62, "num_c1 wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1SFSIC_id, "long_name", 59, "num_c1 wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1SFSIS_id, "long_name", 59, "num_c1 wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1SFWET_id, "long_name", 37, "num_c1 wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1TBF_id, "long_name", 36, "num_c1 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_sfcsiz1_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_sfcsiz1_id, "long_name", 43, "num_c1 calcsize number-adjust column source");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_sfcsiz1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_sfcsiz2_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_sfcsiz2_id, "long_name", 41, "num_c1 calcsize number-adjust column sink");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_sfcsiz2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_sfcsiz3_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_sfcsiz3_id, "long_name", 54, "num_c1 calcsize aitken-to-accum adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_sfcsiz3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_sfcsiz4_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_sfcsiz4_id, "long_name", 54, "num_c1 calcsize accum-to-aitken adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_sfcsiz4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_sfgaex2_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_sfgaex2_id, "long_name", 52, "num_c1 gas-aerosol-exchange renaming column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c1_sfgaex2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_id, "long_name", 21, "num_c2 in cloud water");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2DDF_id, "long_name", 50, "num_c2 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2GVF_id, "long_name", 40, "num_c2 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2SFSBC_id, "long_name", 62, "num_c2 wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2SFSBS_id, "long_name", 62, "num_c2 wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2SFSIC_id, "long_name", 59, "num_c2 wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2SFSIS_id, "long_name", 59, "num_c2 wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2SFWET_id, "long_name", 37, "num_c2 wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2TBF_id, "long_name", 36, "num_c2 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_sfcsiz1_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_sfcsiz1_id, "long_name", 43, "num_c2 calcsize number-adjust column source");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_sfcsiz1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_sfcsiz2_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_sfcsiz2_id, "long_name", 41, "num_c2 calcsize number-adjust column sink");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_sfcsiz2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_sfcsiz3_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_sfcsiz3_id, "long_name", 54, "num_c2 calcsize aitken-to-accum adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_sfcsiz3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_sfcsiz4_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_sfcsiz4_id, "long_name", 54, "num_c2 calcsize accum-to-aitken adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_sfcsiz4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_sfgaex2_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_sfgaex2_id, "long_name", 52, "num_c2 gas-aerosol-exchange renaming column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c2_sfgaex2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3_id, "long_name", 21, "num_c3 in cloud water");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3DDF_id, "long_name", 50, "num_c3 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3GVF_id, "long_name", 40, "num_c3 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3SFSBC_id, "long_name", 62, "num_c3 wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3SFSBS_id, "long_name", 62, "num_c3 wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3SFSIC_id, "long_name", 59, "num_c3 wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3SFSIS_id, "long_name", 59, "num_c3 wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3SFWET_id, "long_name", 37, "num_c3 wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3TBF_id, "long_name", 36, "num_c3 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3_sfcsiz1_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3_sfcsiz1_id, "long_name", 43, "num_c3 calcsize number-adjust column source");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3_sfcsiz1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3_sfcsiz2_id, "units", 6, "#/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3_sfcsiz2_id, "long_name", 41, "num_c3 calcsize number-adjust column sink");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, num_c3_sfcsiz2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1_id, "long_name", 6, "pom_a1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1DDF_id, "long_name", 50, "pom_a1 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1GVF_id, "long_name", 40, "pom_a1 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1SFSBC_id, "long_name", 55, "Wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1SFSBS_id, "long_name", 55, "Wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1SFSIC_id, "long_name", 52, "Wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1SFSIS_id, "long_name", 52, "Wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1SFWET_id, "long_name", 30, "Wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1TBF_id, "long_name", 36, "pom_a1 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1_CLXF_id, "units", 11, "molec/cm2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1_CLXF_id, "long_name", 50, "vertically intergrated external forcing for pom_a1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1_CLXF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1_XFRC_id, "units", 11, "molec/cm3/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1_XFRC_id, "long_name", 27, "external forcing for pom_a1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_a1_XFRC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1_id, "long_name", 21, "pom_c1 in cloud water");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1DDF_id, "long_name", 50, "pom_c1 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1GVF_id, "long_name", 40, "pom_c1 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1SFSBC_id, "long_name", 62, "pom_c1 wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1SFSBS_id, "long_name", 62, "pom_c1 wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1SFSIC_id, "long_name", 59, "pom_c1 wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1SFSIS_id, "long_name", 59, "pom_c1 wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1SFWET_id, "long_name", 37, "pom_c1 wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1TBF_id, "long_name", 36, "pom_c1 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, pom_c1TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_id, "long_name", 6, "so4_a1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1DDF_id, "long_name", 50, "so4_a1 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1GVF_id, "long_name", 40, "so4_a1 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1SFSBC_id, "long_name", 55, "Wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1SFSBS_id, "long_name", 55, "Wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1SFSIC_id, "long_name", 52, "Wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1SFSIS_id, "long_name", 52, "Wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1SFWET_id, "long_name", 30, "Wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1TBF_id, "long_name", 36, "so4_a1 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_CLXF_id, "units", 11, "molec/cm2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_CLXF_id, "long_name", 50, "vertically intergrated external forcing for so4_a1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_CLXF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_XFRC_id, "units", 11, "molec/cm3/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_XFRC_id, "long_name", 27, "external forcing for so4_a1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_XFRC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_sfcoag1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_sfcoag1_id, "long_name", 45, "so4_a1 modal_aero coagulation column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_sfcoag1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_sfcsiz3_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_sfcsiz3_id, "long_name", 54, "so4_a1 calcsize aitken-to-accum adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_sfcsiz3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_sfcsiz4_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_sfcsiz4_id, "long_name", 54, "so4_a1 calcsize accum-to-aitken adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_sfcsiz4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_sfgaex1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_sfgaex1_id, "long_name", 51, "so4_a1 gas-aerosol-exchange primary column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_sfgaex1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_sfgaex2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_sfgaex2_id, "long_name", 52, "so4_a1 gas-aerosol-exchange renaming column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a1_sfgaex2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_id, "long_name", 6, "so4_a2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2DDF_id, "long_name", 50, "so4_a2 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2GVF_id, "long_name", 40, "so4_a2 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2SFSBC_id, "long_name", 55, "Wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2SFSBS_id, "long_name", 55, "Wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2SFSIC_id, "long_name", 52, "Wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2SFSIS_id, "long_name", 52, "Wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2SFWET_id, "long_name", 30, "Wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2TBF_id, "long_name", 36, "so4_a2 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_CLXF_id, "units", 11, "molec/cm2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_CLXF_id, "long_name", 50, "vertically intergrated external forcing for so4_a2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_CLXF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_XFRC_id, "units", 11, "molec/cm3/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_XFRC_id, "long_name", 27, "external forcing for so4_a2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_XFRC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfcoag1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfcoag1_id, "long_name", 45, "so4_a2 modal_aero coagulation column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfcoag1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfcsiz3_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfcsiz3_id, "long_name", 54, "so4_a2 calcsize aitken-to-accum adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfcsiz3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfcsiz4_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfcsiz4_id, "long_name", 54, "so4_a2 calcsize accum-to-aitken adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfcsiz4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfgaex1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfgaex1_id, "long_name", 51, "so4_a2 gas-aerosol-exchange primary column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfgaex1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfgaex2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfgaex2_id, "long_name", 52, "so4_a2 gas-aerosol-exchange renaming column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfgaex2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfnnuc1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfnnuc1_id, "long_name", 57, "so4_a2 modal_aero new particle nucleation column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a2_sfnnuc1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3_id, "long_name", 6, "so4_a3");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3DDF_id, "long_name", 50, "so4_a3 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3GVF_id, "long_name", 40, "so4_a3 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3SFSBC_id, "long_name", 55, "Wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3SFSBS_id, "long_name", 55, "Wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3SFSIC_id, "long_name", 52, "Wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3SFSIS_id, "long_name", 52, "Wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3SFWET_id, "long_name", 30, "Wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3TBF_id, "long_name", 36, "so4_a3 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3_sfgaex1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3_sfgaex1_id, "long_name", 51, "so4_a3 gas-aerosol-exchange primary column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_a3_sfgaex1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1_id, "long_name", 21, "so4_c1 in cloud water");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1AQH2SO4_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1AQH2SO4_id, "long_name", 30, "so4_c1 aqueous phase chemistry");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1AQH2SO4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1AQSO4_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1AQSO4_id, "long_name", 30, "so4_c1 aqueous phase chemistry");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1AQSO4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1DDF_id, "long_name", 50, "so4_c1 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1GVF_id, "long_name", 40, "so4_c1 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1SFSBC_id, "long_name", 62, "so4_c1 wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1SFSBS_id, "long_name", 62, "so4_c1 wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1SFSIC_id, "long_name", 59, "so4_c1 wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1SFSIS_id, "long_name", 59, "so4_c1 wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1SFWET_id, "long_name", 37, "so4_c1 wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1TBF_id, "long_name", 36, "so4_c1 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1_sfcsiz3_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1_sfcsiz3_id, "long_name", 54, "so4_c1 calcsize aitken-to-accum adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1_sfcsiz3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1_sfcsiz4_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1_sfcsiz4_id, "long_name", 54, "so4_c1 calcsize accum-to-aitken adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1_sfcsiz4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1_sfgaex2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1_sfgaex2_id, "long_name", 52, "so4_c1 gas-aerosol-exchange renaming column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c1_sfgaex2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2_id, "long_name", 21, "so4_c2 in cloud water");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2AQH2SO4_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2AQH2SO4_id, "long_name", 30, "so4_c2 aqueous phase chemistry");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2AQH2SO4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2AQSO4_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2AQSO4_id, "long_name", 30, "so4_c2 aqueous phase chemistry");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2AQSO4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2DDF_id, "long_name", 50, "so4_c2 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2GVF_id, "long_name", 40, "so4_c2 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2SFSBC_id, "long_name", 62, "so4_c2 wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2SFSBS_id, "long_name", 62, "so4_c2 wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2SFSIC_id, "long_name", 59, "so4_c2 wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2SFSIS_id, "long_name", 59, "so4_c2 wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2SFWET_id, "long_name", 37, "so4_c2 wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2TBF_id, "long_name", 36, "so4_c2 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2_sfcsiz3_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2_sfcsiz3_id, "long_name", 54, "so4_c2 calcsize aitken-to-accum adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2_sfcsiz3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2_sfcsiz4_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2_sfcsiz4_id, "long_name", 54, "so4_c2 calcsize accum-to-aitken adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2_sfcsiz4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2_sfgaex2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2_sfgaex2_id, "long_name", 52, "so4_c2 gas-aerosol-exchange renaming column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c2_sfgaex2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3_id, "long_name", 21, "so4_c3 in cloud water");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3AQH2SO4_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3AQH2SO4_id, "long_name", 30, "so4_c3 aqueous phase chemistry");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3AQH2SO4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3AQSO4_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3AQSO4_id, "long_name", 30, "so4_c3 aqueous phase chemistry");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3AQSO4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3DDF_id, "long_name", 50, "so4_c3 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3GVF_id, "long_name", 40, "so4_c3 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3SFSBC_id, "long_name", 62, "so4_c3 wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3SFSBS_id, "long_name", 62, "so4_c3 wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3SFSIC_id, "long_name", 59, "so4_c3 wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3SFSIS_id, "long_name", 59, "so4_c3 wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3SFWET_id, "long_name", 37, "so4_c3 wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3TBF_id, "long_name", 36, "so4_c3 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, so4_c3TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_id, "long_name", 6, "soa_a1");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1DDF_id, "long_name", 50, "soa_a1 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1GVF_id, "long_name", 40, "soa_a1 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1SFSBC_id, "long_name", 55, "Wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1SFSBS_id, "long_name", 55, "Wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1SFSIC_id, "long_name", 52, "Wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1SFSIS_id, "long_name", 52, "Wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1SFWET_id, "long_name", 30, "Wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1TBF_id, "long_name", 36, "soa_a1 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_sfcoag1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_sfcoag1_id, "long_name", 45, "soa_a1 modal_aero coagulation column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_sfcoag1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_sfcsiz3_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_sfcsiz3_id, "long_name", 54, "soa_a1 calcsize aitken-to-accum adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_sfcsiz3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_sfcsiz4_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_sfcsiz4_id, "long_name", 54, "soa_a1 calcsize accum-to-aitken adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_sfcsiz4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_sfgaex1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_sfgaex1_id, "long_name", 51, "soa_a1 gas-aerosol-exchange primary column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_sfgaex1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_sfgaex2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_sfgaex2_id, "long_name", 52, "soa_a1 gas-aerosol-exchange renaming column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a1_sfgaex2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_id, "long_name", 6, "soa_a2");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2DDF_id, "long_name", 50, "soa_a2 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2GVF_id, "long_name", 40, "soa_a2 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2SFSBC_id, "long_name", 55, "Wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2SFSBS_id, "long_name", 55, "Wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2SFSIC_id, "long_name", 52, "Wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2SFSIS_id, "long_name", 52, "Wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2SFWET_id, "long_name", 30, "Wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2TBF_id, "long_name", 36, "soa_a2 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_sfcoag1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_sfcoag1_id, "long_name", 45, "soa_a2 modal_aero coagulation column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_sfcoag1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_sfcsiz3_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_sfcsiz3_id, "long_name", 54, "soa_a2 calcsize aitken-to-accum adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_sfcsiz3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_sfcsiz4_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_sfcsiz4_id, "long_name", 54, "soa_a2 calcsize accum-to-aitken adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_sfcsiz4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_sfgaex1_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_sfgaex1_id, "long_name", 51, "soa_a2 gas-aerosol-exchange primary column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_sfgaex1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_sfgaex2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_sfgaex2_id, "long_name", 52, "soa_a2 gas-aerosol-exchange renaming column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_a2_sfgaex2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1_id, "long_name", 21, "soa_c1 in cloud water");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1DDF_id, "long_name", 50, "soa_c1 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1GVF_id, "long_name", 40, "soa_c1 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1SFSBC_id, "long_name", 62, "soa_c1 wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1SFSBS_id, "long_name", 62, "soa_c1 wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1SFSIC_id, "long_name", 59, "soa_c1 wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1SFSIS_id, "long_name", 59, "soa_c1 wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1SFWET_id, "long_name", 37, "soa_c1 wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1TBF_id, "long_name", 36, "soa_c1 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1_sfcsiz3_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1_sfcsiz3_id, "long_name", 54, "soa_c1 calcsize aitken-to-accum adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1_sfcsiz3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1_sfcsiz4_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1_sfcsiz4_id, "long_name", 54, "soa_c1 calcsize accum-to-aitken adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1_sfcsiz4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1_sfgaex2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1_sfgaex2_id, "long_name", 52, "soa_c1 gas-aerosol-exchange renaming column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c1_sfgaex2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2_id, "units", 5, "kg/kg");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2_id, "long_name", 21, "soa_c2 in cloud water");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2DDF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2DDF_id, "long_name", 50, "soa_c2 dry deposition flux at bottom (grav + turb)");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2DDF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2GVF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2GVF_id, "long_name", 40, "soa_c2 gravitational dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2GVF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2SFSBC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2SFSBC_id, "long_name", 62, "soa_c2 wet deposition flux (belowcloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2SFSBC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2SFSBS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2SFSBS_id, "long_name", 62, "soa_c2 wet deposition flux (belowcloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2SFSBS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2SFSIC_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2SFSIC_id, "long_name", 59, "soa_c2 wet deposition flux (incloud, convective) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2SFSIC_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2SFSIS_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2SFSIS_id, "long_name", 59, "soa_c2 wet deposition flux (incloud, stratiform) at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2SFSIS_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2SFWET_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2SFWET_id, "long_name", 37, "soa_c2 wet deposition flux at surface");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2SFWET_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2TBF_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2TBF_id, "long_name", 36, "soa_c2 turbulent dry deposition flux");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2TBF_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2_sfcsiz3_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2_sfcsiz3_id, "long_name", 54, "soa_c2 calcsize aitken-to-accum adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2_sfcsiz3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2_sfcsiz4_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2_sfcsiz4_id, "long_name", 54, "soa_c2 calcsize accum-to-aitken adjust column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2_sfcsiz4_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2_sfgaex2_id, "units", 7, "kg/m2/s");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2_sfgaex2_id, "long_name", 52, "soa_c2 gas-aerosol-exchange renaming column tendency");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, soa_c2_sfgaex2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, wat_a1_id, "units", 1, "m");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, wat_a1_id, "long_name", 36, "aerosol water, interstitial, mode 01");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, wat_a1_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, wat_a2_id, "units", 1, "m");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, wat_a2_id, "long_name", 36, "aerosol water, interstitial, mode 02");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, wat_a2_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, wat_a3_id, "units", 1, "m");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, wat_a3_id, "long_name", 36, "aerosol water, interstitial, mode 03");
    check_err(stat,__LINE__,__FILE__);
    }

    {
    stat = nc_put_att_text(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp, wat_a3_id, "cell_methods", 10, "time: mean");
    check_err(stat,__LINE__,__FILE__);
    }


    /* leave define mode */
    stat = nc_enddef (camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp);
    check_err(stat,__LINE__,__FILE__);

    /* assign variable data */

    stat = nc_close(camrun_PERIOD_cam2_PERIOD_h0_PERIOD_ed_grp);
    check_err(stat,__LINE__,__FILE__);
    return 0;
}
