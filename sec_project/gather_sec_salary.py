
""" In this script there are a variety of scripts that were used t
    to pull the employee median salary and CEO salary
    from SEC file archives.

    Written by Blake Barr in Fall 2018.

"""


import requests
import bs4
import re
import datetime
import pandas as pd
import argparse


FORM = 'DEF 14A'
BASE_URL = "https://www.sec.gov/cgi-bin/srch-edgar"
DF_COLS = ['company', 'filing_year', 'median_salary', 'ceo_salary']


def process_pay_section(pay_section_text):
    pay_section = pay_section_text.strip("\n")
    # Slim spaces of more than one character to just one
    pay_section = ' '.join(pay_section.split())
    pay_section = pay_section.replace('$ ', '$')
    pay_section = pay_section.replace(' : ', ':')
    for s in ["&", "#", "&nbsp;", "nbsp", "\n", ';']:
        pay_section = pay_section.replace(s, "")
    return pay_section


def get_ceo_mult_from_ratios(pay_section):
    """ Gets the CEO Multiplier from pay ratio between median employee salary
        and CEO salary
    """

    # This regex expressions looks for either ratios of 35:1 or 35 to 1
    ratios = re.findall('\d+[,d+]?\.?\d*(?::|[\s-]to[\s-])1', pay_section)
    ratios = [r.replace(',', '') for r in ratios]

    # If we did not find any of the above, the other format I saw was 35 times that of
    if not ratios:
        ratios = re.findall('[(]?\d+\.?\d*[)]? times that', pay_section)

    # Otherwise need to extract the CEO multiplier
    else:
        ratio_str = ratios[0]
        if ':' in ratio_str:
            ceo_mult = float(ratio_str[0:ratio_str.find(':')])
        elif 'to' in ratio_str:
            to_find = '-to-' if '-' in ratio_str else ' to '
            ceo_mult = float(ratio_str[0:ratio_str.find(to_find)])
        elif 'times that' in ratio_str:
            ratio_str = ratio_str.replace('(', '').replace(')', '')
            ceo_mult = float(ratio_str[0:ratio_str.find('times')])
        else:
            ceo_mult = None
            
        return ceo_mult


def get_emp_ceo_sal_from_mult(string_salaries, ceo_mult):
    """ Given a list of salaries and the CEO multiplier, it then seeks to find the CEO salary and
        median salary using that multiplier

        Note: definitely hackish but seemed to be the cleanest way to get thse data
    """

    num_salaries = [float(sal.replace('$', '').replace(',', '')) for sal in string_salaries]
    num_salaries.sort()
     
    # Use CEO Mult to get Employee Salary
    emp_salary = -1
    ceo_salary = -1
    len_salaries = len(num_salaries)
    for i in range(len_salaries - 1):
        for j in range(i + 1, len_salaries):
            lower_sal = num_salaries[i]
            higher_sal = num_salaries[j]
            if abs(higher_sal / lower_sal - ceo_mult) < 2:
                ceo_salary = higher_sal
                emp_salary = lower_sal
        
    if len_salaries == 2 and emp_salary == -1 and ceo_salary == -1:
        emp_salary = num_salaries[0]
        ceo_salary = num_salaries[1]
                
    return emp_salary, ceo_salary


def process_single_page(table_of_filings, sal_df, first_filing_date, verbose):
    num_filings = len(table_of_filings)
    
    # Loop through every entry in table
    for i in range(1, num_filings):
        row = table_of_filings[i]
        cols = row.findAll("td")
        company_name = cols[1].get_text()
        filing_date = datetime.datetime.strptime(cols[4].get_text(), "%m/%d/%Y").date()
    
        # Only grab those filings that have been posted since we last pulled
        if filing_date >= first_filing_date:
            txt_url = cols[2].find()['href']
            comp_def14a_html = requests.get('https://www.sec.gov/{txt_url}'.format(txt_url=txt_url)).text
            comp_def14a = re.sub('<[^<]+?>', '', comp_def14a_html)
             # Remove TOC
            comp_def14a = comp_def14a[int(0.20 * len(comp_def14a)): ]
        
            # If it doesn't have median or pay ratio section move on
            if comp_def14a.find("median") == -1 or (comp_def14a.find('Pay Ratio') == -1 
                                                    and comp_def14a.find('PAY RATIO') == -1):
                continue
            else:
                pay_ratio_start  = re.search('[CEO]?Pay Ratio[\sDisclosure]?', comp_def14a, re.IGNORECASE).start()           
                # Process Pay Section so I can get what I need
                pay_section = process_pay_section(comp_def14a[pay_ratio_start:pay_ratio_start + 5000])

                ceo_mult = get_ceo_mult_from_ratios(pay_section)
                if ceo_mult is None:
                    if verbose:
                       print(company_name)
                       print('Could not get Ratio')
                else:
                     # Get All Saries
                    all_salaries = re.findall('\$\d{1,3},\d{3,3},?\d*', pay_section)
                    emp_salary, ceo_salary = get_emp_ceo_sal_from_mult(all_salaries, 
                                                                   ceo_mult)
                    if emp_salary == -1 and ceo_salary == -1:
                        if verbose:
                            print(company_name)
                            print('Couldnt find salaries to match ratio')
                            print(ceo_mult)
                            print(all_salaries)
                    else:
                        sal_df = sal_df.append(pd.DataFrame([[company_name, filing_date.year,
                                                                   emp_salary, ceo_salary]],
                                               columns=DF_COLS))
    
    return sal_df
    

def final_processing_of_df(df):
    """ Get rid of outliers and final processing of the dataframe containing
        the saliaries
    """
    df.loc[:, 'ratio'] = df.loc[:, 'median_salary'] / df.loc[:, 'ceo_salary']
    df = df.loc[df['ratio'] <= 0.8, :]
    del df['ratio']
    return df


def build_df(first_filing_date, verbose):
    # LOOP THROUGH ALL PAGES
    count = 100
    pos = 1
    sal_df = pd.DataFrame(columns=DF_COLS)
    page = 1
    more_pages = True
    first_year = first_filing_date.year
    # These is some bug with pages need to figure out where it truly ends
    while page < 100:
        print('Page', page)
        url = "{base_url}?text={form}&start={pos}&count={count}&first={first_year}".format(base_url=BASE_URL,
                                                                                           form=FORM,
                                                                                           pos=pos,
                                                                                           count=count,
                                                                                           first_year=first_year)
        list_of_company_forms = requests.get(url)
        soup = bs4.BeautifulSoup(list_of_company_forms.text, 'html.parser')
        table_of_filings = (soup.findAll("table")[4]).findAll("tr")
        sal_df = process_single_page(table_of_filings, sal_df, first_filing_date, verbose)
        # Ex pos is 1 on page 1, will be 101 on page 2 if count is 100
        pos += count
        page += 1

    return final_processing_of_df(sal_df)


def scrape_sec_salary_data(file_name, append_or_write, first_filing_date, verbose):
    salary_df = build_df(first_filing_date, verbose)
    with open(file_name, append_or_write) as f:
        # Only add to header if writing to file the first time
        salary_df.to_csv(f, header=append_or_write == 'w', index=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Script for SEC Salary Scraping')
    parser.add_argument('-f', '--file_name', help='File Name to Store Output', type=str, required=True)
    parser.add_argument('-w',
                        '--write_type',
                        help='a (append to existing file) or w (write to new file)',
                        type=str,
                        choices=['a', 'w'],
                        required=True)
    parser.add_argument('-a',
                        '--after_date',
                        help='Grab Filings after this date (mm/dd/yyyy). Default is 01/01/2018',
                        type=str,
                        default='01/01/2018',
                        required=False)
    parser.add_argument('-v',
                        '--verbose',
                        help='Will print failures if y is put in',
                        type=str,
                        choices=['y', 'n'],
                        default='n',
                        required=False)

    args = parser.parse_args()
    first_filing_date = datetime.datetime.strptime(args.after_date, "%m/%d/%Y").date()

    args = parser.parse_args()
    scrape_sec_salary_data(args.file_name, args.write_type, first_filing_date, args.verbose == 'y')



