package test;

public class s1mme_bearer {
    public Short bearer_id;
    public Short bearer_type;
    public Short bearer_oci;
    public Short bearer_status;
    public Integer bearer_request_cause;
    public Short bearerrequestcausegroup;
    public Short bearerrequestcausespecific;
    public Integer bearer_failure_cause;
    public Short bearerfailurecausegroup;
    public Short bearerfailurecausespecific;
    public Long bearer_enb_gtp_teid;
    public Long bearer_sgw_gtp_teid;

    public void init() {
        bearer_id = new Short((short) 1);
        bearer_type = 1;
        bearer_oci = 1;
        bearer_status = 1;
        bearer_request_cause = 1;
        bearerrequestcausegroup = 1;
        bearerrequestcausespecific = 1;
        bearer_failure_cause = 1;
        bearerfailurecausegroup = 1;
        bearerfailurecausespecific = 1;
        bearer_enb_gtp_teid = 1L;
        bearer_sgw_gtp_teid = 1L;
    }
}