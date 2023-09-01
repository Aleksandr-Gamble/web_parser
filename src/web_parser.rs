use std::{cmp::PartialEq, error::Error, fmt, vec::Vec};
use serde::{Serialize, Deserialize};
use url::Url;



#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct  ParsedDomain {    
    pub phrase: String,         // i.e. "bluecorp" for "bluecorp.co.uk"
    pub tsld: String,           // i.e. ".co.uk" for "bluecorp.co.uk"
}

impl ParsedDomain {
    pub fn to_str(&self) -> String {
        format!("{}{}", self.phrase, self.tsld)
    }
}



#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct ParsedSubDomain {
    pub domain: ParsedDomain,
    pub sub_prefix: String, // i.e. Some("investors") for "investors.bluecorp.co.uk"
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct ParsedEmail {
    pub subdomain: ParsedSubDomain, // i.e. for @hr.company.com
    pub name_prefix: String, // i.e. for jane.doe 
}


/// This struct captuers common http www "schemes"
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum SchemeWWW {
    Http,       // http://
    Https,      // https://
    HttpWWW,    // http://www.
    HttpsWWW,   // https://www.
    HttpWW2,    // http://ww2.
    HttpsWW2,   // https://ww2
    HttpWWW2,   // http://www2.
    HttpsWWW2,  // https://www2.
}


impl SchemeWWW {
    pub fn to_str(&self) -> &'static str {
        match *self {
            SchemeWWW::Http => "http://",
            SchemeWWW::Https => "https://",
            SchemeWWW::HttpWWW => "http://www.",
            SchemeWWW::HttpsWWW => "https://www.",
            SchemeWWW::HttpWW2 => "http://ww2.",
            SchemeWWW::HttpsWW2 => "https://ww2.",
            SchemeWWW::HttpWWW2 => "http://www2.",
            SchemeWWW::HttpsWWW2 => "https://www2.",
        }
    }

    pub fn from_str(text: &str) -> Result<Self, UrlParseError> {
        let scheme = match text {
            "http://" => SchemeWWW::Http,
            "https://" => SchemeWWW::Https,
            "http://www." => SchemeWWW::HttpWWW,
            "https://www." => SchemeWWW::HttpsWWW,
            "http://ww2." => SchemeWWW::HttpWW2,
            "https://ww2." => SchemeWWW::HttpsWW2,
            "http://www2." => SchemeWWW::HttpWWW2,
            "https://www2." => SchemeWWW::HttpsWWW2,
            _ => return Err(UrlParseError{details: format!("{} is not a recognized scheme!", text)})
        };
        Ok(scheme)
    }
}


impl ParsedSubDomain {

    pub fn to_str(&self) -> String {
        format!("{}{}{}", self.sub_prefix, self.domain.phrase, self.domain.tsld)
    }

    pub fn from_str(subdomain: &str) -> Result<Self, UrlParseError> {
        // given a subdomain string (with the www already dropped), Decompose it into a subdomain
        let subdomain_lowercase = subdomain.trim().to_lowercase(); // for matching TSLD
        // try second level domains first becasue they "include" top-level domains
        for tsld in SECOND_LEVEL_DOMAINS.iter() {
            if subdomain_lowercase.ends_with(tsld) {
                return ParsedSubDomain::from_str_and_tsld(&subdomain_lowercase, tsld)
            }
        }
        // then try top-level domains
        for tsld in TOP_LEVEL_DOMAINS.iter() {
            if subdomain_lowercase.ends_with(tsld) {
                return ParsedSubDomain::from_str_and_tsld(&subdomain_lowercase, tsld)
            }
        }
        // if you reach this point, you might have a bad url
        return Err(UrlParseError{details:format!("Unable to identify a known top or second-level domain in '{}'", subdomain)})
    }

    fn from_str_and_tsld(subdomain_lowercase: &str, tsld: &'static str) -> Result<Self, UrlParseError> {
        // if you know a subdomain ends with a top/second level domain, this will split it and return a SubDomain
        if !subdomain_lowercase.ends_with(tsld) {
            return Err(UrlParseError{details:format!("The subdomain '{}' does not end with the specified tsld '{}'", subdomain_lowercase, tsld)})
        }
        let pre_tsld = subdomain_lowercase.replace(tsld, "");
        let sp: Vec<&str> = pre_tsld.split(".").collect(); // split everythng before the TSLD
        let spl = sp.len();
        let (sub_prefix, phrase) = match spl {
            0 => return Err(UrlParseError{details: format!("nothing but the top/second level domain '{}' was provided!", subdomain_lowercase)}),
            1 => (String::new(), pre_tsld), // None indicates there is no subdomain prefix
            _ => {
                let dp = sp[spl - 1 .. spl].join(""); // should only be lenght 1 anyway
                let mut sp = sp[0..spl - 1].join(".");
                sp.push_str(".");
                (sp, dp)
            },
        };
        if phrase.len() == 0 {
            return Err(UrlParseError{details: format!("you provided an empty domain with '{}'!", subdomain_lowercase)})
        }
        if &sub_prefix.len() > &63 {
            println!("sub_prefix.len() >63 is suspect");
            return Err(UrlParseError{details: "sub_prefix.len() >63 is suspect".to_string()})
        }
        if phrase.len() > 100 {
            // This helps with VARCHAR constraints but also avoids suspiciously long entries
            println!("phrase.len() >100 is suspect");
            return Err(UrlParseError{details: "phrase.len() >100 is suspect".to_string()})
        }
        let domain = ParsedDomain{phrase, tsld: tsld.to_string()};
        Ok(ParsedSubDomain{
            domain: domain,
            sub_prefix: sub_prefix,
        })
    }

}



impl ParsedEmail {

    pub fn to_str(&self) -> String {
        format!("{}@{}", self.name_prefix, self.subdomain.to_str())
    }

    pub fn from_str(email: &str) -> Result<Self, UrlParseError> {
        if email.matches("@").count() != 1 {
            return  Err(UrlParseError{details: format!("'{}' should contain exactly one '@'!", email)})
        }
        let sp: Vec<&str> = email.split("@").collect();
        let name_prefix = match sp.get(0) {
            Some(val) => val.to_lowercase().trim().to_string(),
            None => return Err(UrlParseError{details: "how did you not have a name prefix?".to_string()})
        };
        if name_prefix.len() > 63 {
            // This helps with VARCHAR constraints but also avoids suspiciously long prefixes
            println!("name_prefix.len() >63 is suspect");
            return Err(UrlParseError{details: "name_prefix.len() >63 is suspect".to_string()})
        }
        let subdomain = match sp.get(1) {
            Some(val) => ParsedSubDomain::from_str(val)?,
            None => return Err(UrlParseError{details: "how did you not have a subdomain?".to_string()})
        };
        Ok(ParsedEmail{
            subdomain: subdomain,
            name_prefix: name_prefix,
        })
    }
    
}


impl std::str::FromStr for ParsedEmail {
    type Err = UrlParseError;

    fn from_str(email: &str) -> Result<Self, Self::Err> {
        ParsedEmail::from_str(email)
    }
}


#[derive(Serialize, Deserialize, Debug)]
pub struct ParsedUrl {
    pub scheme_www: SchemeWWW,
    pub subdomain: ParsedSubDomain, 
    pub path: String, // i.e. "/products/homedecor"
    pub sorted_query: String, // i.e. "?color=blue&size=7" - SORTED and LOWERCASE
}




impl ParsedUrl {

    pub fn querypath(&self) -> String {
        // return a concatenation of the path and the sorted_query. This is the last part of a URL
        format!("{}{}", self.path, self.sorted_query)
    }

    pub fn to_str(&self) -> String {
        format!("{}{}{}", self.scheme_www.to_str(), self.subdomain.to_str(), self.querypath())
    }


    pub fn qp_len(&self) -> i16 {
        // return the length of the querypath. Shorter URLs within a subdomain are typically more meaningful
        (self.path.len() + self.sorted_query.len()) as i16
    }


    // this gives the portable hash of the querypath as an i32 (overflows start back at 0)
    // This is MUCH smaller to store and index than the actual querypath, and the probability of collision is low
    pub fn qp_h32(&self) -> i32 {
        crate::utils::hash_str_i32(&self.querypath())
    }


    pub fn from_str(link: &str) -> Result<Self, UrlParseError> {
        // attempt to create a WebUrl from a (string) link/url
        let mut link = link.trim().to_lowercase();
        if !link.contains(".") {
            return Err(UrlParseError{details:format!("The url '{}' does not contain a period.", link)})
        }
        if !link.starts_with("http") {
            // allow 'sloppy' user input like just "company.com"
            link = format!("http://{}", link);
        }
        // attempt to parse the link
        let url = Url::parse(&link)?;
        // if you reach this point, the url library was able to parse it. But can you resolve a subdomain?
        let mut subdom_str = match url.domain() {
            //note that this library uses "domain" for what is called a "subdomain" elsewhere in this file
            Some(val) => val.to_string(),
            None => return  Err(UrlParseError{details:format!("A subdomain could not be resolved in'{}'", &link)}),
        };
        let mut scheme_www_str = url.scheme().to_string(); // typically this starts as http:// or https://
        scheme_www_str.push_str("://");
        for www in ["www.", "ww2.", "www2."].iter() {
            if subdom_str.starts_with(www) {
                scheme_www_str.push_str(www);
                subdom_str = subdom_str.replace(www, "");
            }
        }
        let scheme_www = SchemeWWW::from_str(&scheme_www_str)?;
        let subdomain = ParsedSubDomain::from_str(&subdom_str)?;
        let mut path = url.path().to_string();
        if path.ends_with("/") {
            let mut chars = path.chars();
            chars.next_back();
            path = chars.as_str().to_string();
        }
        if path.len() > 512 {
            return Err(UrlParseError{details: "path > 254 characters!".to_string()})
        }
        let sorted_query = match url.query() {
            None => String::new(),
            Some(query) => {
                // a query was discovered, but you need to sort its arguments
                let mut args = Vec::new();
                for qp in query.split("&") {
                    args.push(qp);
                }
                args.sort();
                format!("?{}", args.join("&"))
            }
        };
        if sorted_query.len() > 254 {
            // while not technically invalid, this is uncommonly big
            return Err(UrlParseError{details: "sorted_query > 254 characters!".to_string()})
        }
        Ok(ParsedUrl {scheme_www, subdomain, path, sorted_query })
    }

    pub fn extract_all(text: &str) -> Vec<Self> {
        let text = text.replace("("," ").replace(")"," ").replace(". ", " "); // perhaps the user enclosed a link in parenthesis
        let mut links = Vec::new();
        // check each word in the text to see if it is a link
        for word in text.split_whitespace() {
            match ParsedUrl::from_str(word) {
                Ok(link) => links.push(link),
                Err(_) => {}
            }
        }
        links
    }
}



impl std::str::FromStr for ParsedUrl {
    type Err = UrlParseError;

    fn from_str(surl: &str) -> Result<Self, Self::Err> {
        ParsedUrl::from_str(surl)
    }
}


/// The trait DomainRoot extracts the domain components from a domain, subdomain, url, etc
pub trait DomainRoot {
    fn dom_phrase(&self) -> &str;
    fn dom_tsld(&self) -> &str;
}

impl DomainRoot for ParsedDomain {
    fn dom_phrase(&self) -> &str { &self.phrase }
    fn dom_tsld(&self) -> &str { &self.tsld }
}

impl DomainRoot for ParsedSubDomain {
    fn dom_phrase(&self) -> &str { self.domain.dom_phrase() }
    fn dom_tsld(&self) -> &str { self.domain.dom_tsld() }
}

impl DomainRoot for ParsedEmail {
    fn dom_phrase(&self) -> &str { self.subdomain.dom_phrase() }
    fn dom_tsld(&self) -> &str { self.subdomain.dom_tsld() }
}

impl DomainRoot for ParsedUrl {
    fn dom_phrase(&self) -> &str { self.subdomain.dom_phrase() }
    fn dom_tsld(&self) -> &str { self.subdomain.dom_tsld() }
}


#[derive(Debug, PartialEq)]
pub struct UrlParseError {
    details: String
}


impl Error for UrlParseError {
    fn description(&self) -> &str {
        &self.details
    }
}


impl fmt::Display for UrlParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,"{}",self.details)
    }
}

impl From<url::ParseError> for UrlParseError {
    fn from(err: url::ParseError) -> UrlParseError {
        UrlParseError{details: err.to_string()}
    }
}


static TOP_LEVEL_DOMAINS: [&'static str; 177] = [".ky", ".lb", ".lk", ".mc", ".np", ".aero", ".kr", ".ug", ".top", ".mk", ".cf", ".mil", ".link", ".gov", ".do", ".in", ".ir", ".nr", ".mobi", 
    ".ec", ".nyc", ".fj", ".sd", ".pk", ".bb", ".bo", ".er", ".gr", ".pr", ".jobs", ".kw", ".gn", ".de", ".se", ".edu", ".sh", ".ao", ".iq", ".ng",
    ".ro", ".sb", ".life", ".co", ".nz", ".com", ".th", ".agency", ".xyz", ".it", ".ki", ".bs", ".ph", ".pw", ".yu", ".scot", ".et", ".za", ".ml", 
    ".net", ".live", ".gt", ".blog", ".coop", ".club", ".es", ".tw", ".ye", ".mx", ".af", ".gg", ".zm", ".dz", ".pe", ".cat", ".shop", ".one", 
    ".eg", ".ru", ".mn", ".work", ".km", ".au", ".tn", ".mw", ".tj", ".jo", ".tr", ".ca", ".bz", ".dev", ".tt", ".tech", ".nl", ".br", ".bh", ".st", 
    ".tz", ".cn", ".org", ".kh", ".ps", ".ga", ".sl", ".ck", ".tk", ".my", ".je", ".mv", ".fun", ".ar", ".kz", ".sy", ".al", ".sn", ".kn", ".pro", 
    ".ly", ".mt", ".asia", ".mg", ".id", ".me", ".rw", ".fk", ".biz", ".sv", ".lr", ".mo", ".nf", ".lv", ".vi", ".qa", ".mu", ".cy", ".sc", ".py", 
    ".ke", ".na", ".sa", ".ac", ".pl", ".at", ".uy", ".gu", ".cr", ".sg", ".rs", ".gh", ".site", ".om", ".bn", ".pt", ".ae", ".us", ".icu", ".uk", 
    ".ua", ".mz", ".vn", ".re", ".jp", ".ba", ".ve", ".news", ".fr", ".il", ".hk", ".pa", ".today", ".info", ".sz", ".ma", ".ni", ".name", ".int",
    ".eu", ".io"];


static SECOND_LEVEL_DOMAINS: [&'static str; 1454] = [
    ".kemerovo.ru", ".gov.sg", ".edu.gr", ".ac.ma", ".name.fj", ".tver.ru", ".org.tr", ".ac.nz", ".aero.mv", ".plc.ly", ".mil.rw", ".info.nr", ".tourism.tn",
    ".info.ki", ".nyc", ".mil.bo", ".nnov.ru", ".med.om", ".com.af", ".edu.za", ".asso.re", ".tm.za", ".com.vi", ".net.qa", ".sch.ng", ".chukotka.ru", 
    ".nat.tn", ".gov.st", ".incheon.kr", ".dn.ua", ".org.er", ".co.com", ".go.tj", ".nome.pt", ".pro.ae", ".com.gt", ".p.se", ".shop", ".db.za", ".info.ve", 
    ".hi.cn", ".com.gh", ".com.ph", ".ac.ir", ".arts.nf", ".gov.bn", ".w.er", ".vladikavkaz.ru", ".org.au", ".tatarstan.ru", ".edu.er", ".com.er", ".pro", 
    ".go.id", ".lg.ua", ".odessa.ua", ".web.tj", ".web.tr", ".mil.ni", ".fnd.br", ".edu.pr", ".gov.mo", ".jobs.tt", ".psc.br", ".edu.es", ".org.gt", ".edu.dz",
    ".ne.ug", ".edu.bh", ".edu.it", ".gov.dz", ".cr", ".com.pt", ".ltd.lk", ".gob.es", ".perso.sn", ".ed.jp", ".site", ".lublin.pl", ".belau.pw", ".net.iq", 
    ".com.al", ".mpm.school.za", ".ne.us", ".kamchatka.ru", ".mil.gt", ".notaires.km", ".edu.ph", ".pro.om", ".edu.bb", ".info.tr", ".org.nz", ".art.sn", 
    ".defense.tn", ".org.af", ".gc.ca", ".sn.cn", ".mil.ac", ".joshkar-ola.ru", ".am.br", ".edu.sg", ".org.za", ".co.ke", ".gov.in", ".net.pe", ".tv.tr",
    ".presse.km", ".edu.ba", ".presse.ml", ".biz.et", ".mil.tj", ".gen.in", ".co.bb", ".com.bn", ".bourse.za", ".name.cy", ".org.gh", ".nom.pe", ".o.se",
    ".co.kr", ".ltd.cy", ".net.lk", ".embaixada.st", ".gov.ph", ".plc.uk", ".gov.bs", ".sh.cn", ".lel.br", ".tv.sd", ".gov.mz", ".rec.nf", ".scot.uk", 
    ".parti.se", ".edu.ly", ".net.gt", ".gov.tj", ".chernovtsy.ua", ".gov.kn", ".edu.lr", ".vn.ua", ".adygeya.ru", ".web.pk", ".net.tj", ".edu.pa", ".abo.pa",
    ".art.dz", ".test.tj", ".gov.sa", ".com.bb", ".sld.pe", ".mil.st", ".cybernet.za", ".org.pt", ".kirovograd.ua", ".net.pr", ".gov.kz", ".com.ps", ".mat.br", ".co.sh", ".olsztyn.pl", ".pharmaciens.km", ".parliament.nz", ".edu.ng", 
    ".tambov.ru", ".soc.uk", ".net.nr", ".gob.ve", ".hn.cn", ".ncape.school.za", ".pe.ca", ".edu.ni", ".daejeon.kr", ".b.br", ".net.al", ".edu.ml", ".asso.dz", ".biz.mv", "edu.bz", ".tum.de", ".sch.ly", ".edu.co", ".pp.ua", ".fr",
    ".tj.cn", ".org.lk", ".museum.mw", ".surgut.ru", ".jus.br", ".lviv.ua", ".gov.tw", ".pro.mv", ".gov.mu", ".gob.mx", ".net.ml", ".blog.br", ".name.mk", ".its.me", ".org.bo", ".net.cn", ".org.nr", ".name.my", ".store.ro", 
    ".donetsk.ua", ".pskov.ru", ".co.ua", ".chungnam.kr", ".ac.fj", ".com.kh", ".med.ly", ".org.my", ".mil.eg", ".uzhgorod.ua", ".mil.kr", ".sumy.ua", ".nsk.ru", ".intl.tn", ".edu.sy", ".u.se", ".assn.lk", ".ln.cn", ".pro.mk",
    ".cat", ".com.sc", ".org.lb", ".omsk.ru", ".org.dz", ".or.ug", ".org.tw", ".school.za", ".publ.pt", ".ind.tn", ".vrn.ru", ".mil.my", ".if.ua", ".org.sl", ".com.gn", ".gov.gr", ".national-library-scotland.uk", ".org.kn", 
    ".org.mw", ".prd.mg", ".gov.mw", ".pro.fj", ".edu.sd", ".net.mx", ".in.rs", ".com.iq", ".gov.me", ".arts.ro", ".gon.pk", ".pub.sa", ".gdansk.pl", ".wroclaw.pl", ".org.cy", ".mil.ec", ".org.uy", ".org.bb", ".school.nz", 
    ".mil.kz", ".ngo.pl", ".gov.mn", ".nf.ca", ".edu.bs", ".org.ru", ".design", ".imb.br", ".org.np", ".qsl.br", ".net.ph", ".aero", ".ac.cn", ".agric.za", ".nom.fk", ".gov.iq", ".id.ly", ".biz.cy", ".khabarovsk.ru", 
    ".nom.mg", ".org.mx", ".org.cn", ".com.mt", ".net.je", ".net.vn", ".sld.do", ".net.ma", ".yuzhno-sakhalinsk.ru", ".jobs", ".store.nf", ".org.sc", ".edu.jo", ".mincom.tn", ".co.cr", ".bpt.me", ".mordovia.ru", ".org.vn", 
    ".com.mo", ".edu.uy", ".amur.ru", ".gov.sd", ".g12.br", ".kids.us", ".fed.us", ".xyz", ".mob.ki", ".net.bh", ".net.pl", ".org.ki", ".ecn.br", ".club", ".te.ua", ".org.gr", ".art.do", ".gb.net", ".org.sv", ".rochest.er", 
    ".sch.my", ".tech", ".edu.lv", ".jet.uk", ".imt.za", ".chuvashia.ru", ".ms.kr", ".ac.jp", ".game.tw", ".net.jo", ".go.jp", ".mil.er", ".net.gn", ".iaccess.za", ".gouv.fr", ".co.in", ".inf.br", ".ecape.school.za", ".net.mt",
    ".arq.br", ".com.de", ".net.ec", ".gob.pa", ".jor.br", ".edu.pt", ".inf.mk", ".net.lv", ".co.tj", ".net.cy", ".com.sa", ".ivanovo.ru", ".news", ".com.ve", ".altai.ru", ".csiro.au", ".edu.ky", ".grozny.ru", ".health.nz", 
    ".per.nf", ".name.eg", ".est.pr", ".int.pt", ".co.vu", ".ens.tn", ".ac.ni", ".ac.fk", ".izhevsk.ru", ".ind.in", ".gouv.rw", ".cld.bz", ".mil.do", ".ro.com", ".edu.kw", ".or.jp", ".co.ni", ".i.ph", ".ngo.za", ".org.qa", 
    ".od.ua", ".ltd.ye", ".dnepropetrovsk.ua", ".edu.iq", ".eu.com", ".com.sd", ".fj.cn", ".net.ac", ".me.ye", ".maori.nz", ".ac.gn", ".com.kz", ".sch.uk", ".gov.ba", ".cn.ua", ".ind.gt", ".mil.lv", ".res.in", ".info.fj", 
    ".org.gn", ".khmelnitskiy.ua", ".org.in", ".tv.br", ".ac.za", ".cq.cn", ".qc.ca", ".gov.ir", ".hl.cn", ".net.ir", ".edu.nr", ".mil.iq", ".mod.uk", ".org.om", ".com.nf", ".k12.il", ".smolensk.ru", ".rs.ba", ".edu.yu", ".sch.jo",
    ".org.mz", ".co.gg", ".nom.ro", ".fin.tn", ".net.ar", ".ac.pa", ".org.ck", ".co.vi", ".mil.pl", ".edu.my", ".com.vn", ".vladivostok.ru", ".jeonnam.kr", ".net.sh", ".vladimir.ru", ".mil.id", ".gov.zm", ".com.uy", ".info.ke", 
    ".mil.py", ".edu.kn", ".com.bs", ".net.af", ".eng.br", ".or.kr", ".kazan.ru", ".co.tz", ".qh.cn", ".int.mw", ".gov.ki", ".info", ".sc.ke", ".web.lk", ".sc.kr", ".consulado.st", ".tm.se", ".torun.pl", ".edu.mv", ".gov.cy",
    ".ac.in", ".com.eg", ".net.tt", ".com.sl", ".ad.jp", ".com.mu", ".org.sz", ".ha.cn", ".jar.ru", ".ernet.in", ".busan.kr", ".store.bb", ".biz.ua", ".ac.cy", ".coop.tt", ".nm.cn", ".org.do", ".tomsk.ru", ".co.st", ".org.ly",
    ".szczecin.pl", ".net.tw", ".gda.pl", ".rovno.ua", ".net.sd", ".gov.do", ".com.tj", ".pa.us", ".press.ma", ".pro.cy", ".fam.pk", ".prd.fr", ".kzn.school.za", ".edu.ec", ".av.tr", ".chita.ru", ".mil.ve", ".gob.sv", 
    ".com.ac", ".co.ck", ".co.sz", ".int.mv", ".net.pk", ".medecin.km", ".c.se", ".net.uy", ".sch.ir", ".org.ve", ".cri.nz", ".com.pl", ".parliament.cy", ".gov.sy", ".travel.tt", ".kharkov.ua", ".edu.tj", ".go.tz", ".int.tt", 
    ".flog.br", ".coop.mv", ".int.rw", ".co.ve", ".hs.kr", ".oh.us", ".ptz.ru", ".isa.us", ".gov.br", ".press.se", ".com.tr", ".lea.uk", ".alt.za", ".daegu.kr", ".nt.ca", ".net.gr", ".org.sh", ".n.se", ".org.jo", ".art.pl", 
    ".gov.et", ".com.ml", ".int.ar", ".info.ck", ".iwi.nz", ".a.se", ".edu.zm", ".grp.lk", ".not.br", ".lipetsk.ru", ".mil.mv", ".museum.mv", ".com.cn", ".gov.jo", ".g.se", ".zj.cn", ".mobi", ".edu.mo", ".com.cy", ".org.yu", 
    ".tm.ro", ".edu.lk", ".co.mz", ".univ.sn", ".com.om", ".name.jo", ".ac.mw", ".police.uk", ".mus.br", ".net.nz", ".info.bh", ".net.ua", ".nom.ni", ".com.bo", ".co.ug", ".net.py", ".gov.hk", ".gov.ml", ".edu.sn", ".sec.ps", 
    ".pro.pr", ".com.gu", ".mari-el.ru", ".ah.cn", ".org.iq", ".mari.ru", ".gov.gn", ".nx.cn", ".grondar.za", ".pb.ao", ".ubc.ca", ".co.za", ".name.vn", ".net.ye", ".id.lv", ".org.me", ".vologda.ru", ".net.zm", ".pp.se",
    ".name.tt", ".com.sh", ".edu.pk", ".novosibirsk.ru", ".org.lv", ".jeonbuk.kr", ".edu.eg", ".gov.lr", ".biz.vn", ".gov.ec", ".gov.yu", ".co.th", ".cnt.br", ".mobi.ng", ".net.my", ".r.se", ".tom.ru", ".pro.br", ".mil.sy", ".e.se", ".com.br", 
    ".katowice.pl", ".chungbuk.kr", ".kuban.ru", ".gs.cn", ".net.sy", ".com.mx", ".name.tr", ".crimea.ua", ".perm.ru", ".gov.lk", ".pe.kr", ".fst.br", ".kostroma.ru", ".ks.ua", ".cat.tt", ".tm.cy", ".com.mw", ".org.se", ".org.vi", ".tsk.tr", ".org.sa", 
    ".unmo.ba", ".sd.cn", ".or.id", ".org.sn", ".co.at", ".gov.nr", ".net.kw", ".com.mv", ".com.rw", ".gov", ".com.ly", ".sakhalin.ru", ".stavropol.ru", ".info.bb", ".mil.in", ".int.vn", ".org.ac", ".com.do", ".asso.fr", ".com.zm", ".nom.fr", "org.bz",
    ".volgograd.ru", ".com.re", ".edu.mx", ".ac.rw", ".info.sd", ".ac.zm", ".ato.br", ".info.tt", ".net.uk", ".com.lb", ".com.sn", ".gov.tt", ".koenig.ru", ".krakow.pl", ".mb.ca", ".rns.tn", ".coop", ".global", ".org.ng", ".com.qa", ".on.ca", ".net.om",
    ".khv.ru", ".org.ec", ".org.bh", ".gos.pk", ".lg.jp", ".store.st", ".org.rs", ".biz.ck", ".co.us", ".org.kw", ".co.ba", ".biz.fj", ".biz.tj", ".gov.pk", ".lugansk.ua", ".ac.kr", ".pol.tr", ".com.fj", ".conf.lv", ".af.mil", ".co.ir", ".ne.jp", 
    ".go.kr", ".kchr.ru", ".radom.pl", ".edu.mg", ".ternopil.ua", ".city.za", ".www.ro", ".fi.cr", ".nov.ru", ".etc.br", ".edu.gt", ".gz.cn", ".rnd.ru", ".mk.ua", ".com.jo", ".edu.pl", ".co.om", ".y.se", ".gov.ua", ".ac.tz", ".govt.uk", 
    ".org.mn", ".fin.ec", ".com.nr", ".ivano-frankivsk.ua", ".no.com", ".com.ru", ".pro.ec", ".og.ao", ".wcape.school.za", ".gov.ng", ".net.hk", ".net.th", ".biz.pr", ".bialystok.pl", ".edu.sa", ".dni.us", ".com.lk", ".us.org", ".mil.np", 
    ".mil.uy", ".com.pe", ".gov.ye", ".web.za", ".pwr.pl", ".coop.br", ".priv.me", ".gov.sc", ".fot.br", ".penza.ru", ".com.ky", ".lodz.pl", ".gov.au", ".me.uk", ".mil.nz", ".stv.ru", ".com.pa", ".psi.br", ".net.lr", ".ing.pa", ".ngo.lk", ".gov.ru",
    ".gyeongbuk.kr", ".edu.sv", ".olivetti.za", ".rv.ua", ".yakutia.ru", ".yekaterinburg.ru", ".nls.uk", ".gob.gt", ".chernigov.ua", ".zaporizhzhe.ua", ".med.sa", ".gov.pt", ".ebiz.tw", ".ac.se", ".war.net.id", ".mobi.ke", ".web.id", ".de.ki", ".travel", 
    ".gwangju.kr", ".yk.ca", ".presse.fr", ".gen.tr", ".plo.ps", ".de.com", ".net.tr", ".gov.tr", ".biz", ".mil.ar", ".mil.co", ".gouv.km", ".com.ro", ".kh.ua", ".edu.vn", ".web.do", ".kursk.ru", ".org.pr", ".edu.ps", ".gr.jp", ".gorzow.pl", ".t.se", 
    ".name.mv", ".bj.cn", ".com.dz", ".edu.al", ".edu.rs", ".org.il", ".net.sg", ".int.lk", ".irkutsk.ru", ".l.se", ".edu.me", ".agr.br", ".org.ky", ".org.mu", ".ac.mu", ".edu.ck", ".net.bs", ".edu.cn", ".kr", ".gov.pl", ".org.uk", ".ck.ua", ".biz.nr", 
    ".unsa.ba", ".in", ".com.gr", ".cv.ua", ".net.ki", ".net.ru", ".id.au", ".british-library.uk", ".mil.gh", ".zhitomir.ua", ".org.mt", ".co.pw", ".com.hk", ".edu.om", ".or.pw", ".4fd.in", ".gov.gu", ".net.il", ".net.ng", ".ekloges.cy", ".com.ye", ".org.ma",
    ".nom.za", ".cim.br", ".sk.ca", ".udm.ru", "net.bz", ".gen.ck", ".it.ao", ".co.je", ".re.kr", ".net.do", ".mosreg.ru", ".ulsan.kr", ".org.es", ".net.id", ".gov.mk", ".org.fj", ".asso.km", ".cng.br", ".co.rs", ".trd.br", ".magadan.ru", ".med.br", 
    ".nic.tj", ".biz.ng", ".net.tn", ".adm.br", ".ed.cr", ".tel.tr", ".chel.ru", ".info.ec", ".khakassia.ru", ".gov.er", ".com.py", ".sx.cn", ".he.cn", ".mil.km", ".net.gu", ".go.ug", ".rec.br", ".com.ng", ".pro.tt", ".co.fk", ".org.mk", ".jx.cn", 
    ".gop.pk", ".zgora.pl", ".odo.br", ".saratov.ru", ".net.mk", ".net.za", ".js.cn", ".edu.af", ".gov.fk", ".nl.ca", ".gv.at", ".srv.br", ".lnk.to", ".gov.my", ".top", ".gok.pk", ".yamal.ru", ".co.mu", ".mil", ".idv.hk", ".gob.pk", ".ir", ".com.ba",
    ".sch.sa", ".org.ni", ".edu.bn", ".gob.pe", ".org.ps", ".red.sv", ".net.ni", ".org.zm", ".nu.ca", ".prof.pr", ".media", ".bbs.tr", ".med.pa", ".net.in", ".net.sl", ".net.mo", ".tsk.ru", ".gob.ar", ".f.se", ".name.et", ".org.kh", ".org.mv", ".or.mu", ".net.pt", 
    ".gov.uk", ".net.np", ".nom.sh", ".bio.br", ".ac.pr", ".info.ro", ".agrinet.tn", ".samara.ru", ".gov.ae", ".mil.gr", ".kiev.ua", ".idv.tw", ".per.kh", ".gx.cn", ".edu.pe", ".vyatka.ru", ".udmurtia.ru", ".co.nz", ".com.km", ".msk.ru", ".marine.ru", ".mil.ae", 
    ".com.fr", ".net.co", ".or.tz", ".edu.mt", ".edu.sh", ".museum.om", ".d.se", ".com.na", "gov.bz", ".principe.st", ".net.st", ".cc.bh", ".bl.uk", ".org.mo", ".asn.lv", ".es.kr", ".ngo.ph", ".ac.sz", ".gov.il", ".org.gg", ".int.tj", ".org.pe", ".net.mu",
    ".museum.tt", ".net.ly", ".edu.mz", ".ac.at", ".edu.au", ".sld.pa", ".net.sb", ".edu.ve", ".net.kh", ".ed.pw", ".info.nf", ".biz.tt", ".xj.cn", ".coop.km", ".nb.ca", ".astrakhan.ru", ".gov.co", ".pol.dz", ".unbi.ba", ".net.mw", ".me.ua", ".waw.pl", 
    ".ns.ca", ".edu.bo", ".orgn.uk", ".veterinaire.km", ".bmd.br", ".gov.pr", ".gov.ck", ".com.lr", ".mi.th", ".org.sg", ".in.th", ".ind.br", ".bc.ca", ".tur.ar", ".edu.hk", ".nikolaev.ua", ".co.tt", ".spb.ru", ".biz.ki", ".edu.ua", ".firm.ro", ".nis.za", 
    ".name.tj", ".nom.pa", ".net.lb", ".biz.tr", ".net.nf", ".4fd.us", ".arkhangelsk.ru", ".gov.bo", ".edu.gh", ".chelyabinsk.ru", ".ac.ae", ".gov.al", ".biz.pl", ".mil.mg", ".firm.in", ".net.kz", ".sch.om", ".info.vn", ".com.ar", ".coop.mw", ".nsn.us", 
    ".warszawa.pl", ".asia", ".info.pr", ".ppg.br", ".net.dz", ".edu.gu", ".info.tj", ".gov.gh", ".gov.qa", ".edu.sl", ".in.ua", ".org.st", ".biz.bh", ".ac.il", ".edu.kh", ".org.py", ".nw.school.za", ".int.ru", ".k12.vi", ".pp.ru", ".ltd.uk", ".edu.kz",
    ".co.me", ".nom.re", ".com.et", ".in.us", ".co.ae", ".ac.vn", ".gov.lv", ".ac.rs", ".komi.ru", ".com.sv", ".zp.ua", ".store", ".name.ng", ".edu.lb", ".edu.et", ".net.br", ".tm.fr", ".vet.br", ".or.at", ".dp.ua", ".esp.br", ".com.tt", ".kalmykia.ru", 
    ".ntr.br", ".org.ug", ".online", ".other.nf", ".org.ro", ".sch.ae", ".com.mk", ".co.jp", ".net.bo", ".nom.br", ".nic.uk", ".uk.com", ".ac.uk", ".net.er", ".gangwon.kr", ".k12.tr", ".co.il", ".org.je", ".co.na", ".biz.om", ".lp.school.za", ".dnssec.ir",
    ".co.zm", ".xz.cn", ".com.ua", ".km", ".sch.lk", ".go.cr", ".dagestan.ru", ".gob.do", ".ind.er", ".org.tn", ".edu.do", ".sch.id", ".nhs.uk", ".pix.za", ".press.cy", ".com.lv", ".edu.rw", ".law.za", ".net.ae", ".org.hk", ".gov.bh", ".gd.cn", ".net.bb",
    ".sa.cr", ".tula.ru", ".kurgan.ru", ".edunet.tn", ".co.id", ".gov.sb", ".go.ke", ".org.tt", ".net.gg", ".mil.om", ".ac.mz", ".or.cr", ".gov.mt", ".poznan.pl", ".nom.es", ".ac.ru", ".edu.ki", ".club.tw", ".aero.tt", ".gov.af", ".sci.eg", ".mil.qa", ".org.al",
    ".tel.ki", ".ma", ".news.sy", ".kv.ua", ".gen.nz", ".fs.school.za", ".kg.kr", ".tv.bo", ".org.br", ".health.vn", ".gv.ao", ".int.bo", ".gp.school.za", ".muni.il", ".edu.ru", ".tm.mg", ".tur.br", ".org.sy", ".x.se", ".ac.cr", ".info.mv", ".go.pw", ".edu", 
    ".gov.mg", ".com.pk", ".gov.mv", ".gov.ar", ".ne.kr", ".tsaritsyn.ru", ".co.rw", ".mil.ph", ".edu.br", ".or.ke", ".jeju.kr", ".com.es", ".edu.tr", ".govt.nz", ".s.se", ".co.uk", ".com.ec", ".tyumen.ru", ".org.sb", ".com.au", ".tm.km", ".net.ky", ".gov.py", 
    ".ryazan.ru", ".km.ua", ".com.bh", ".unze.ba", ".zlg.br", ".com.tn", ".nt.ro", ".ulan-ude.ru", ".org.ae", ".org.ir", ".landesign.za", ".gov.ac", ".web.ve", ".com.st", ".world", ".sc.cn", ".gov.ma", ".art.br", ".co.ye", ".com.ni", ".kirov.ru", ".oryol.ru", ".ac.me", 
    ".gob.ni", ".com.sg", ".w.se", ".in.net", ".cym.uk", ".org.ua", ".edu.in", ".info.pl", ".gov.kw", ".bel.tr", ".com.ki", ".org.pk", ".net.sc", ".mobi.tt", ".tel.tt", ".gov.rw", ".ed.ao", ".com.my", ".plc.ye", ".cherkassy.ua", ".kranoyarsk.ru", ".org.bn", ".gov.ly", 
    ".net.pa", ".md.us", ".slg.br", ".org.mg", ".nel.uk", ".int", ".ac.id", ".z.se", ".med.ec", ".gov.kh", ".edu.sc", ".gov.sh", ".org.ba", ".net.fk", ".murmansk.ru", ".com.mg", ".mil.ng", ".mil.cn", ".info.tn", ".dr.tr", ".ac.ke", ".ac.om", ".kr.ua", ".mil.uk", ".gov.om", 
    ".org.co", ".wiki.br", ".net.vi", ".net.au", ".name.ae", ".net.fj", ".org.tj", ".bryansk.ru", ".cbg.ru", ".iq", ".ac.ug", ".gov.vn", ".com.np", ".perso.tn", ".vlog.br", ".idf.il", ".edu.mn", ".per.sg", ".fm.br", ".net.kn", ".gov.tn", ".mil.tt", ".edu.np", ".tuva.ru", 
    ".net.ve", ".hotel.lk", ".med.sd", ".bashkiria.ru", ".soc.lk", ".isla.pr", ".net.bn", ".rnrt.tn", ".inca.za", ".mil.kh", ".gov.ky", ".untz.ba", ".net.ps", ".id.ir", ".mil.tw", ".poltava.ua", ".gov.eg", ".slupsk.pl", ".asn.au", ".adv.br", ".edu.km", ".edu.sb", ".geek.nz",
    ".far.br", ".org.eg", ".mil.jo", ".mil.br", ".net.me", ".buryatia.ru", ".org.bs", ".i.se", ".com.co", ".com.sy", ".or.th", ".vinnica.ua", ".co.ma", ".name.pr", ".gov.lb", ".zt.ua", ".go.th", ".org.pl", ".lutsk.ua", ".ac.tj", ".gov.sl", ".yn.cn", ".edu.tt", ".co.yu",
    ".rec.ro", ".edu.mw", ".org.sd", ".gov.ps", ".org.et", ".me.ke", ".edu.qa", ".nom.co", ".gob.bo", ".org.kz", ".gov.bb", ".com.kw", ".mil.pe", ".wroc.pl", ".saotome.st", ".mil.fj", ".mil.al", ".org.ar", ".ab.ca", ".b.se", ".edu.tw",
    ".sebastopol.ua", ".gyeongnam.kr", ".seoul.kr", ".web.nf", ".orenburg.ru", ".nalchik.ru", ".ac.th", ".org.fk", ".icnet.uk", ".co.mw", ".ne.pw", ".idn.sg", ".sch.zm", ".net.rw", ".net.eg", ".bir.ru", ".biz.bb", ".edu.mk", ".hb.cn",
    ".eun.eg", ".life", ".tm.mc", ".mil.za", ".tw.cn", ".tmp.br", ".simbirsk.ru", ".ac.yu", ".eti.br", ".edu.st", ".blog", ".gov.za", ".asso.mc", ".gov.it", ".gyeonggi.kr", ".go.com", ".net.ba", ".firm.nf", ".com.tw", ".e-burg.ru", 
    ".org.gu", ".gouv.sn", ".net.ck", ".org.ye", ".org.ph", ".gov.cn", ".tv.bb", ".mil.ru", "com.bz", ".info.et", ".biz.pk", ".sc.ug", ".gub.uy", ".belgorod.ru", ".net.sa", ".ne.tz", ".kherson.ua", ".mil.ba", ".edu.py", ".kaluga.ru",
    ".m.se", ".gov.np", ".org.ml", ".gov.rs", ".nic.in", ".net.et", ".k.se", ".com.sb", ".com.pr", ".bd.se", ".edu.ar", ".ne.ke", ".nom.km", ".co.ao", ".net.mv", ".ggf.br", ".org.lr", ".pro.vn", ".us.com", ".pl.ua", ".karelia.ru",
    ".jl.cn", ".voronezh.ru", ".rnu.tn", ".h.se", ".org.pa", ".name", ".mi.us", ".parliament.uk"
    ];

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn verify_hash_i32() {
        // this test verifies you still get the same qp_h32 for a given url as you did with heritage code
        let url1 = ParsedUrl::from_str(" www.learn.com/wiki/postgresql ").unwrap();
        assert_eq!(url1.qp_h32(), 87963596);
        let url2 = ParsedUrl::from_str("http://ww2.monkey.werx.co.uk/").unwrap();
        assert_eq!(url2.qp_h32(), 628087993);
        let url3 = ParsedUrl::from_str("http://something.org/p/bnx4xghbg9b?utm_source=ig_twitter_share&igshid=ixojfrpfbsyp").unwrap();
        assert_eq!(url3.qp_h32(), 44291727);
    }
    #[test]
    fn resolve_some_subdomains() {
        // ensure you can resolve subdomains from strings properly
        let s1 = ParsedSubDomain::from_str("SHop.coolProducts.Tv.Bo").unwrap();
        assert_eq!(s1.domain.to_str(), "coolproducts.tv.bo".to_string());
        assert_eq!(s1.sub_prefix, "shop.".to_string());
        assert_eq!(s1.domain.phrase, "coolproducts".to_string());
        assert_eq!(s1.domain.tsld, ".tv.bo".to_string());
        let s2 = ParsedSubDomain::from_str("bluestuff.Co.uk").unwrap();
        assert_eq!(s2.domain.to_str(), "bluestuff.co.uk".to_string());
        assert_eq!(s2.sub_prefix, String::new());
        assert_eq!(s2.domain.phrase, "bluestuff".to_string());
        assert_eq!(s2.domain.tsld, ".co.uk".to_string());
    }
    #[test]
    #[should_panic]
    fn subdom_must_contain_tsld() {
        // ensure an error is thrown trying to call SubDomain::from_str_and_tsld with a mismatched tsld
        let _s = ParsedSubDomain::from_str_and_tsld("something.com", ".name").unwrap();
    }
    #[test]
    fn test_force_failure_sudomains() {
        // ensure none of this jibberish resolves to a subdomain
        for bad in ["p93jroij23@#@#!@orij23r", "good morning", "http 12345", "", ".", ".com", ".gov.ps"].iter() {
            let x = match ParsedSubDomain::from_str(bad) {
                Ok(_) => "resolved",
                Err(_) => "failed",
            };
            assert_eq!(x, "failed");
        }
    }
    #[test]
    fn hyperlink() {
        // verify you can correctly parse a complicated url
        for link in [
            "WW2.sILly.InVesTorS.UaOrg.flog.BR/aBout/interactive?sIZE=77&colour=blue#123",
            "http://sILly.InVesTorS.UaOrg.flog.BR/aBout/interactive?sIZE=77&colour=blue#123"].iter() {
            let url = ParsedUrl::from_str(link).unwrap();
            assert_eq!(url.subdomain.to_str(), "silly.investors.uaorg.flog.br".to_string());
            assert_eq!(url.subdomain.domain.tsld, ".flog.br".to_string());
            assert_eq!(url.subdomain.domain.phrase, "uaorg".to_string());
            assert_eq!(url.querypath(), "/about/interactive?colour=blue&size=77".to_string()); // Notice these are sorted alphabetically
            assert_eq!(url.subdomain.sub_prefix, "silly.investors.".to_string());
        }
        // a simpler example
        let url = ParsedUrl::from_str("https://www.symo.co.uk?query=blue").unwrap();
        assert_eq!(url.subdomain.to_str(), "symo.co.uk".to_string());
        assert_eq!(url.subdomain.domain.to_str(), "symo.co.uk".to_string());
        assert_eq!(url.subdomain.domain.phrase, "symo".to_string());
        assert_eq!(url.subdomain.domain.tsld, ".co.uk".to_string());
        assert_eq!(url.querypath(), "?query=blue".to_string());
        assert_eq!(url.subdomain.sub_prefix, String::new());
        assert_eq!(url.scheme_www, SchemeWWW::HttpsWWW);
        // obscure domain
        let url = ParsedUrl::from_str("https://www2.some.more.stuff.company.ltd.uk/now").unwrap();
        assert_eq!(url.subdomain.domain.to_str(), "company.ltd.uk".to_string());
        assert_eq!(url.subdomain.sub_prefix, "some.more.stuff.".to_string());
        assert_eq!(url.scheme_www, SchemeWWW::HttpsWWW2 );
    }
    #[test]
    fn test_force_failure_weburls() {
        // ensure none of this jibberish resolves to a subdomain
        for bad in ["p93jroij23@#@#!@orij23r", "good morning", "http 12345", "", ".", ".com", ".gov.ps"].iter() {
            let x = match ParsedUrl::from_str(bad) {
                Ok(_) => "resolved",
                Err(_) => "failed",
            };
            assert_eq!(x, "failed");
        }
    }
    #[test]
    fn more_hyperlink_fails() {
        // ensure you get errors where you expect them
        let urlstr = "www.overstock.com";
        let result = ParsedUrl::from_str(urlstr);
        match result {
            Ok(val) => {},
            Err(e) => panic!("{} was expected to parse properly!", urlstr)
        }
        let urlstr = "www.overstock.con"; // ".con" is not a valid TLD
        let result = ParsedUrl::from_str(urlstr);
        match result {
            Ok(val) => panic!("{} does not have a valid Top-Level Domain!", urlstr),
            Err(e) => {},
        }
    }
    #[test]
    fn test_to_str() {
        // ensure you can resolve urls from strings, then get the strings back again
        for link in [
            "http://www.mysite.org/stuff/tchotchkies?query=blue".to_string(),
            "https://investors.somecompany.com/home.aspx".to_string()
        ] {
            let url = ParsedUrl::from_str(&link).unwrap();
            assert_eq!(link, url.to_str());
        }
    }
    #[test]
    fn resolve_emails() {
        let em = ParsedEmail::from_str("boB.Dole@dolebananas.com").unwrap();
        assert_eq!(em.name_prefix, "bob.dole".to_string());
        assert_eq!(em.subdomain.domain.phrase, "dolebananas".to_string());
        let em = ParsedEmail::from_str("boB.Dole@hr.SomeOrg.co.Uk ").unwrap();
        assert_eq!(em.subdomain.sub_prefix, "hr.".to_string());
    }
    #[test]
    #[should_panic]
    fn bad_email_1() {
        let em = ParsedEmail::from_str("boB.Dole@dole@bananas.com").unwrap();
    }
    #[test]
    #[should_panic]
    fn bad_email_2() {
        let em = ParsedEmail::from_str("sup.com").unwrap();
    }

}

