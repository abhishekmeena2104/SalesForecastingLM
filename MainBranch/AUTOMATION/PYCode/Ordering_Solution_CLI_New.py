# -*- coding: utf-8 -*-
"""
Created on Mon Nov  6 14:13:51 2017

@author: harsh.ratde
"""

import os
import sys
import datetime
import time
import subprocess

root = "/shared/sasdata02/ACOE_DEV/ordering_solution/ACoE_Ordering/"
os.chdir(root)
output_dest = 'Ordering_Execution'

def make_folder(root_f,output_dest_f):
    if not os.path.exists(root_f+output_dest_f):
        os.makedirs(root_f+output_dest_f)
        os.system('chmod 777 '+root_f+output_dest_f) 
        print('------------------------------------------------------')
        print '\n Folder created: ',root_f+output_dest_f,' \n'
        print('------------------------------------------------------')
    else:
        print('------------------------------------------------------')
        print '\n Folder already exist: ',root_f+output_dest_f,' \n' 
        print('------------------------------------------------------')
        
make_folder(root,output_dest)


# check the argument to decide the interactive or automated mode
if sys.argv[1].upper() == 'AUTO':
    mode = 'AUTO'
elif sys.argv[1].upper() == 'MANUAL':
    mode = 'MANUAL'
else:
    print 'Improper arguments passed' 
    quit()
#   mode of executin has been decided, and exception will be raised in case of 
#   improper arguments passed and code will stop executing 

#-------------------------------------------------------------------------------------
#       environment mode for code execution
#-------------------------------------------------------------------------------------
if mode == 'AUTO':
    try:
        env = sys.argv[2].upper()
    except:
        env = 'TEST'
    
    print 'Environment selected : ',env

elif mode == 'MANUAL':
    look_up_env = {'1':'TEST',
                   '2':'PROD',
                   '3':'DEV'}
    def choice_inp():               
        chk = str(input('Choose the environment to run the code: \n1-TEST \n2-PROD \n3-DEV \n'))
        return chk

    try:
        choice = choice_inp()
    except:
        choice = '0'
    while choice.strip() not in ['1','2','3']:
        print('invalid option selected, please reselect the option for environment\n')
        try:
            choice = choice_inp()
        except:
            choice = '0'
    env = look_up_env[choice]
    print 'Environment selected : ',env
    
make_folder(root,output_dest+'/'+env)
#-------------------------------------------------------------------------------------    

def input_date(guide):
    global today,prev_day
    if guide == 'AUTO':
        option_selected = 1
    elif guide == 'MANUAL':

        if cncpt == 'HC':
            def today_hc_forc_date():
                

                input_date1 = str(input('Please provide the today date of forecast. \n Custom date should be provided in YYYYMMDD format: \n')) 
                print 'Selected todays forecast date is :' +input_date1+'\n'

                verify_prev_dt = str(input('Do you want to change the today date of forecast,\n1 - Dont want to change \n2 - Yes want to change\n'))

                if verify_prev_dt == '2':
                    today = today_hc_forc_date()
                else:
                    print '\n-------------------------------------------------'
                    print 'selected input_date is :',input_date1
                    print '-------------------------------------------------\n'
                    return input_date1
            today = today_hc_forc_date()

            def prev_hc_forc_date():

                input_date1 = str(input('Please provide the previous date of forecast. \n Custom date should be provided in YYYYMMDD format: \n')) 
                print 'Selected prev forecast date is :' +input_date1+'\n'

                verify_prev_dt = str(input('Do you want to change the previous date of forecast,\n1 - Dont want to change \n2 - Yes want to change\n'))

                if verify_prev_dt == '2':
                    prev_day = prev_hc_forc_date()           
                else:
                    print '\n-------------------------------------------------'
                    print 'selected input_date is :',input_date1
                    print '-------------------------------------------------\n'
                    return input_date1

            input_prev_case = str(input('Do you want to provide previous date: \n1 - Yes\n2 - No\n '))

            if input_prev_case == '1':
                prev_day = prev_hc_forc_date()   
            elif input_prev_case == '2':
                prev_day =''
            print 'prev_day is :',prev_day,':'

                

        else:

            chk = 0
            while chk == 0:
                try:
                    option_selected = str(input('Choose the date(option number) to execute the code: \n1-Today \n2-Custom \n'))
                except:
                    option_selected = '0'
                if option_selected not in ['1','2']:
                    print 'Invalid option selected, please choose 1 or 2\n'
                    print '-----------------------------------------------'
                else:
                    chk = 1
            del chk

            
            
            if int(option_selected) == 1:
                today = str(datetime.datetime.today().year*10000+datetime.datetime.today().month*100+datetime.datetime.today().day)
                print 'Today selected as: ', today
                
            elif int(option_selected) ==2:
                case_a = 0 
                case_b = 0
                while case_a + case_b <2 :
                    
                    today = str(input('Custom date should be provided in YYYYMMDD format: \n'))
                    #today = 20171106
                    if int(today) <= datetime.datetime.today().year*10000+datetime.datetime.today().month*100+datetime.datetime.today().day:
                        case_a = 1
                    else:
                        case_a = 0
                        print('input date is greater than todays date, which is invalid entry.\n The maximum allowed date is latest date \n TRY AGAIN!')
                    
                    def existing_date(path):
                        #path = root+output_dest+'/'+env
                        try:
                            #logic 1 - takes max of all directories
                            #out_date = int(max([item for item in os.listdir(path) if len(str(int(item)))==8]))

                            #logic 2 - takes max from only the folder with the same cncpt and dept
                            folder_list=[]
                            for item in os.listdir(path):
                                if len(str(int(item)))==8:
                                    for cncpt_itr in os.listdir(path+'/'+item):
                                        if cncpt == cncpt_itr:
                                            for dept_itr in os.listdir(path+'/'+item+'/'+cncpt_itr):
                                                if dept == dept_itr:
                                                    folder_list.append(item)

                            out_date = int(max(folder_list))
                            


                        except:
                            out_date = 20000001
                        
                        return out_date
                        
                    if int(today) >=existing_date(root+output_dest+'/'+env):
                        case_b = 1
                    else:
                        case_b = 0
                        print 'input date is less than existing dates, which is invalid entry.\n The \
                                allowed date should be greater than or equal to existing folder \n TRY AGAIN!,existing max date is: ',existing_date(root+output_dest+'/'+env)
                    
                
        make_folder(root,output_dest+'/'+env+'/'+today)
        



#-----------------------------------------------------------------------------
#   date folder created         
#-----------------------------------------------------------------------------

cncpt_map={
            '1':{
                    'Short name'    :'MAX',
                    'Full name'     :'Max',
                    'dept':{  '1'   :['CLN','Children'],
                              '2'   :['FTQ','Footware'],
                              '3'   :['LDS','Ladies'],
                              '4'   :['LNG','Lingerie'],
                              '5'   :['MEN','Mens'],
                              '6'   :['HOME','Home'],
                              '7'  :['ACC','Accessories']}
                },
            '2':{
                    'Short name'    :'SP',
                    'Full name'     :'Splash',
                    'dept':{  '1'   :['LNG','Lingerie'],
                              '2'   :['BAS','Basic'],
                              '3'   :['YM','Youth Men'],
                              '4'   :['LCP','Lee Cooper'],
                              '5'   :['DNM','Denim'],
                              '6'   :['SNF','Suits & Formals'],
                              '7'   :['BOS','Bossini Men'],
                              '8'   :['XXL','XXL'],
                              '9'   :['ACC','Accessories'],
                              '10'  :['KAP','Kappa M & L'],
                              '11'  :['ACT','Active M & L'],
                              '12'  :['WLP','WLP'],
                              '13'  :['SMW','Smart Women'],
                              '14'  :['ELLE','Elle'],
                              '15'  :['YW','Youth Women']
                          }
                },
             
            '3':{
                    'Short name':'LS',
                    'Full name' :'Lifestyle',
                    'dept':{'1':['COSM','Cosmetics'],
                            '2':['COIM','Cosmetics - IMPORT'],
                            '3':['BATH','Bath'],
                            '4':['BED','Bedroom'],
                            '5':['DIN','Dining'],
                            '6':['KIT','Kitchen'],
                            '7':['LIV','Living']
                          }
                },       

            '4':{
                    'Short name':'HC',
                    'Full name' :'Homecenter',
                    'dept':{
                        '1' : ['FBD','Bedroom'],
                        '2' : ['FDN','Dinning'],
                        '3' : ['FGD','Garden Furniture'],
                        '4' : ['FHS','H O S O'],
                        '5' : ['FLV','Living'],
                        '6' : ['FMT','Mattresses'],
                        '7' : ['FYB','Young Bedroom'],
                        '8' : ['HBT','Bath'],
                        '9' : ['HGD','Garden / Wicker/ Outdoor'],
                        '10' : ['HGF','Gifts'],
                        '11' : ['HIT','Interiors'],
                        '12' : ['HKT','Kitchen Utensils'],
                        '13' : ['HLD','Laundry'],
                        '14' : ['HOH','Other Household'],
                        '15' : ['HSF','Soft Furnishing'],
                        '16' : ['HTT','Table top'],
                        '17' : ['HVD','Video / Audio / Magazine Storage'],
                        '18' : ['HXM','Xmas'],
                        '19' : ['KBT','Bath'],
                        '20' : ['KGD','Garden / Wicker/ Outdoor'],
                        '21' : ['KGF','Gifts'],
                        '22' : ['KIT','Interiors'],
                        '23' : ['KBD','Kids Bedroom'],
                        '24' : ['KKT','Kitchen Utensils'],
                        '25' : ['KLD','Laundry'],
                        '26' : ['KOH','Other Household'],
                        '27' : ['KSF','Soft Furnishing'],
                        '28' : ['KTT','Table top'],
                        '29' : ['KVD','Video / Audio / Magazine Storage'],
                        '30' : ['PIL','PIL']
			}
                },       
            '5':{
                    'Short name':'SM',
                    'Full name' :'Shoemart',
                    'dept':{'1':['SMA','SMA']
                          }
                },                         
            
            '6':{
                    'Short name':'BS',
                    'Full name' :'Babyshop',
                    'dept':{'1':['BA','Basics Apparels'],
                            '2':['CLTG','Clothing Girls'],
                            '3':['CLTE','Clothing Essentials'],
                            '4':['CLTB','Clothing Boys'],
                            '5':['BNA','Basics Non Apparels']
                          }
                },                        
                    
            '7':{
                    'Short name':'SPR',
                    'Full name' :'Spar',
                    'dept':{'1':['SPA','SPA']
                          }
                },
             '8':{
                    'Short name':'IC',
                    'Full name' :'Iconic',
                    'dept':{'1':['MNTD','MNTD'],
                            '2':['LDTD','LDTD']
                          }
                }
           }


source_dict={'1':'File',
             '2':'Interactive interface'}





#while True:
#    sam = str(input('Data to be taken from: \n1-File \n2-Interactive interface \n'))
#    if sam in list(source_dict.keys()):
#        break

sam = '2'
source_file = source_dict[sam]

print 'Mode of input ingestion: ',source_file


if source_file == 'File':
    file = open('Automation_input_sheet.csv')
    
    for num,line in enumerate(file):
        if num == 0:    
            header = line.replace('\n','')
        else:   
            lookup_dict = {}
            #test[][]
            input_date(mode)        

            for i in range(0,len(header.split(','))):
                #print(i)
                lookup_dict[header.split(',')[i]]=line.replace('\n','').split(',')[i]
                #print(lookup_dict)
            
            #   create folder for hierarchy
            if lookup_dict['CNSDR_CASE'] == str(1):
                cncpt   = lookup_dict['CNCPT']
                make_folder(root,output_dest+'/'+env+'/'+today+'/'+cncpt)
                dept    = lookup_dict['dept'] 
                make_folder(root,output_dest+'/'+env+'/'+today+'/'+cncpt+'/'+dept)
                make_folder(root,output_dest+'/'+env+'/'+today+'/'+cncpt+'/'+dept+'/'+'Input')
                make_folder(root,output_dest+'/'+env+'/'+today+'/'+cncpt+'/'+dept+'/'+'Output')
                
elif source_file == 'Interactive interface':       
    
    print('-----------------------------------------------------------------')
    def sortTheDict_key(dict_nm):
        i_list=[]
        for i in list(dict_nm.keys()):
            i_list.append(int(i))
        i_list.sort()
        return i_list
        
    for item in sortTheDict_key(cncpt_map):
        print item,'-',cncpt_map[str(item)][ 'Short name'],' for ',cncpt_map[str(item)]['Full name']
        
    chk_cncpt = 0
    while chk_cncpt ==0:    
        strng = 'Choose the number 1-'+str(len(cncpt_map.keys()))+', to select the concept:\n'
        try:
            cncpt_sl   = str(input(strng))
        except:
            cncpt_sl   = '0'

        if cncpt_sl in cncpt_map.keys():
            chk_cncpt = 1
            break
        else:
            print('Selected option is not in the list of registered concept, please select the concept from below mentioned list:')            
            for item in sortTheDict_key(cncpt_map):
                print item,'-',cncpt_map[str(item)][ 'Short name'],' for ',cncpt_map[str(item)]['Full name']

            
    cncpt   = cncpt_map[cncpt_sl]['Short name']
    print('-------------------------------------------------------')
    print 'Select concept is ',cncpt_map[cncpt_sl]['Full name'],', with short name ',cncpt_map[cncpt_sl]['Short name'] 
    print('-------------------------------------------------------\n\n')    

    for item in sortTheDict_key(cncpt_map[cncpt_sl]['dept']):
        print item,'-',cncpt_map[cncpt_sl]['dept'][str(item)][0],' for ',cncpt_map[cncpt_sl]['dept'][str(item)][1]
    
    chk_dept = 0
    while chk_dept ==0:    
        dep_strng = 'Choose the number 1-'+ str(len(cncpt_map[cncpt_sl]['dept'].keys()))+', to select the depatment:\n'
        try:
            dept_sl   = str(input(dep_strng))
        except:
            dept_sl   = '0'

        if dept_sl in cncpt_map[cncpt_sl]['dept'].keys():
            chk_dept = 1
            break
        else:
            print('Selected option is not in the list of registered department, please select the department from below mentioned list:')            
            for item in sortTheDict_key(cncpt_map[cncpt_sl]['dept']):
                print item,'-',cncpt_map[cncpt_sl]['dept'][str(item)][0],' for ',cncpt_map[cncpt_sl]['dept'][str(item)][1]
    
            
    dept   = cncpt_map[cncpt_sl]['dept'][dept_sl][0]
    del chk_dept

    print('-------------------------------------------------------')
    print 'Select dept is ',cncpt_map[cncpt_sl]['dept'][str(dept_sl)][1],', with short name ',cncpt_map[cncpt_sl]['dept'][str(dept_sl)][0]
    print('-------------------------------------------------------\n\n')
    
    #--------------------------------------------------------------------
    #   Conditions to be checked with the specific department selected
    #--------------------------------------------------------------------
    if cncpt == 'LS' and dept == 'COSM':

        ORD_TYP_OPTN = {'1':'LOC','2':'IMPORT'}
        for item in sortTheDict_key(ORD_TYP_OPTN):
            print item,' - ',ORD_TYP_OPTN[str(item)]

        CHK_ORD_TYP = 0
        while CHK_ORD_TYP ==0:    
            strng = 'Choose the number 1-'+str(len(ORD_TYP_OPTN.keys()))+', to select the concept:\n'
            try:
                ord_typ_sl   = str(input(strng))
            except:
                ord_typ_sl   = '0'

            if ord_typ_sl in ORD_TYP_OPTN.keys():
                CHK_ORD_TYP = 1
                ORD_TYP = ORD_TYP_OPTN[ord_typ_sl]
                break
            else:
                print('Selected option is not in the list of registered concept, please select the concept from below mentioned list:')            
                for item in sortTheDict_key(ORD_TYP_OPTN):
                    print item,' - ',ORD_TYP_OPTN[str(item)]
    else:
        ORD_TYP=   'IMPORT'

    input_date(mode)    

    make_folder(root,output_dest+'/'+env+'/'+today+'/'+cncpt) 
    make_folder(root,output_dest+'/'+env+'/'+today+'/'+cncpt+'/'+dept)
    make_folder(root,output_dest+'/'+env+'/'+today+'/'+cncpt+'/'+dept+'/'+'Input')
    make_folder(root,output_dest+'/'+env+'/'+today+'/'+cncpt+'/'+dept+'/'+'Output')

                        
#----------------------------------------------------------------------------------------
#   Folder is created
#----------------------------------------------------------------------------------------
 
def inputFileChk(path):
    #item = '20171002'
    #today = '20171112'
    #cncpt = 'MX'
    #dept = 'CLN'
    #path = root+output_dest+'/'+env+'/'+item+'/'+cncpt+'/'+dept+'/'+'Input'
    try:
        list_of_file = os.listdir(path)
        input_file = [item for item in list_of_file if ('inputs' in item) and (item.split('.')[1]=='xlsx')]
        #input_file != []
    except:
        input_file=[]
    return input_file


# Option to copy the file from latest folder or not:
#file_copy_optn = 0

if cncpt == 'HC':
    print 'file is in place'
    dest_file_nm = 'null'

else:

    if mode == 'AUTO':
        file_copy_optn = '3' 
    elif mode == 'MANUAL':
        print '------------------------------------------------------'
        file_chk = inputFileChk(root+output_dest+'/'+env+'/'+today+'/'+cncpt+'/'+dept+'/'+'Input/')
        if file_chk == []:
            print 'Input folder :',root+output_dest+'/'+env+'/'+today+'/'+cncpt+'/'+dept+'/'+'Input/',', does not contain any input file.'
            filecopy_choice = '2'
        else:
            print 'Input file: ',file_chk[0] ,'\nis already present in the input folder:',root+output_dest+'/'+env+'/'+today+'/'+cncpt+'/'+dept+'/'+'Input/'
            if len(file_chk) >1:
                print 'There are other files in the input folder'
                for item in file_check:
                    print(item)

                sys.exit('Please clear the input directory and keep only one input file')

            chk = 0
            while chk == 0:

                try:
                    filecopy_choice = str(input('Choose one of the following option:\n1-If you want to keep the input file \n2-if you want to delete the existing file in copy again \n'))
                except:
                    filecopy_choice = '0'

                if filecopy_choice not in ['1','2']:
                    print 'Invalid option selected, please choose 1 or 2\n'
                    print '-----------------------------------------------'
                else:
                    chk = 1
            del chk

            if filecopy_choice == '2':
                comm = 'rm '+root+output_dest+'/'+env+'/'+today+'/'+cncpt+'/'+dept+'/'+'Input/'+file_chk[0]
                os.system(comm)
     







    if filecopy_choice == '2':
        inpu_st = 'Do you want to copy the file from latest existing folder: \n1 - Yes, copy file from latest directory \n2-Dont copy, you will upload the file @ '+root+output_dest+'/'+env+'/'+today+'/'+cncpt+'/'+dept+'/'+'Input \n'
            
        chk = 0
        while chk == 0:
            try:
                file_copy_optn = str(input(inpu_st))
            except:
                file_copy_optn = '0'
            if file_copy_optn not in ['1','2']:
                print 'Invalid option selected, please choose 1 or 2\n'
                print '-----------------------------------------------'
            else:
                chk = 1
        del chk

        #file_copy_optn = str(input(inpu_st))


        if file_copy_optn =='1':
            
    #[][]       
            try:
                folder_list=[]
                for item in os.listdir(root+output_dest+'/'+env):
                    if len(str(int(item)))==8:
                        for cncpt_itr in os.listdir(root+output_dest+'/'+env+'/'+item):
                            if cncpt == cncpt_itr:
                                for dept_itr in os.listdir(root+output_dest+'/'+env+'/'+item+'/'+cncpt_itr):
                                    if dept == dept_itr:
                                        if inputFileChk(root+output_dest+'/'+env+'/'+item+'/'+cncpt+'/'+dept+'/'+'Input')!=[]:
                                            folder_list.append(item)

                out_date = int(max(folder_list))

                #out_date = int(max([item for item in os.listdir(root+output_dest+'/'+env) if ((len(str(int(item)))==8) and (item != today) and (inputFileChk(root+output_dest+'/'+env+'/'+item+'/'+cncpt+'/'+dept+'/'+'Input')!=[])) ]))        
                
                print('-------------------------------------------------------')    
                print 'Latest existing folder is ',out_date,'. ',inputFileChk(root+output_dest+'/'+env+'/'+str(out_date)+'/'+cncpt+'/'+dept+'/'+'Input'),' file will be copied for this run'
                print('-------------------------------------------------------')
                
                #copying the identified file to the desination folder
                command = "cp "+root+output_dest+'/'+env+'/'+str(out_date)+'/'+cncpt+'/'+dept+'/'+'Input'+'/'+inputFileChk(root+output_dest+'/'+env+'/'+str(out_date)+'/'+cncpt+'/'+dept+'/'+'Input')[0]+' '+ root+output_dest+'/'+env+'/'+today+'/'+cncpt+'/'+dept+'/'+'Input'
                os.system(command)
                
            except:
                print('no other folder exist')
                file_copy_optn = '2'
        
        
        
        
         
        if file_copy_optn == '2':
            status_file = 0
            while status_file ==0:
                print 'Waiting for input file to be copied at: ',root+output_dest+'/'+env+'/'+today+'/'+cncpt+'/'+dept+'/'+'Input \n',' \n'
                if inputFileChk(root+output_dest+'/'+env+'/'+today+'/'+cncpt+'/'+dept+'/'+'Input')!=[]:
                    status_file =1
                time.sleep(60)
            print('-------------------------------------------------------')    
            print 'name of the copied file is :',inputFileChk(root+output_dest+'/'+env+'/'+today+'/'+cncpt+'/'+dept+'/'+'Input')
            print('-------------------------------------------------------')



    #--------------------------------------------------------------------------------------------------
    print('-------------------------------------------------------')    
    print('Rename the input file in the input folder')
    print('-------------------------------------------------------')    

    dest_folder = root+output_dest+'/'+env+'/'+today+'/'+cncpt+'/'+dept+'/'+'Input'    
    #rename
    source_file_nm = inputFileChk(root+output_dest+'/'+env+'/'+today+'/'+cncpt+'/'+dept+'/'+'Input')[0]
    dest_file_nm = today+source_file_nm.split(source_file_nm.split('_')[0])[1] 
    comm_mv = 'mv '+dest_folder+'/'+source_file_nm+' '+dest_folder+'/'+dest_file_nm
    os.system(comm_mv)

#-----------------------------------------------------------------------------------------
#running the command
chk = 0
while chk == 0:
    try:
        command_option = str(input('Choose the following Option: \n1-To run the Ordering Code \n2-To abort\n'))
    except:
        command_option = '0'
    if command_option not in ['1','2']:
        print 'Invalid option selected, please choose 1 or 2\n'
        print '-----------------------------------------------'
    else:
        chk = 1
del chk

if command_option =='2':
    sys.exit('Selected to abort the execution\n')
elif  command_option =='1':
    make_folder(root,'Automation/automation_log/')
    log_file_name1 = today+'_'+cncpt+'_'+dept+'_Order_exec1.log'
    lst_file_name1 = today+'_'+cncpt+'_'+dept+'_Order_exec1.lst'

    log_file_name2 = today+'_'+cncpt+'_'+dept+'_Order_exec2.log'
    lst_file_name2 = today+'_'+cncpt+'_'+dept+'_Order_exec2.lst'

    chk = 0
    while chk == 0:
        try:
            usr = str(raw_input('Enter the user prefix(for example: HR for Harsh):\n this prefix should be same as registered with DE team\n')).upper()
        except:
            usr = ''
        if usr == '':
            print 'Invalid name selected, please select a valid prefix\n'
            print '-----------------------------------------------'
        else:
            chk = 1
            print '\n-----------------------------------------------'
            print 'Selected prefix is: ',usr
            print '-----------------------------------------------\n\n'
    del chk

    ref_lookup={    'MAX':{ 
                            'CPT'       :'MX',
                            'CNCPT_KEY' :32,
                            'DVSN_NM'   :'Max',
                            'DVSN_CD'   :6
                            },
                    'HC':{ 
                            'CPT'       :'HC',
                            'CNCPT_KEY' :30,
                            'DVSN_NM'   :'HomeCentre',
                            'DVSN_CD'   :4
                            },
                    'SP':{ 
                            'CPT'       :'SP',
                            'CNCPT_KEY' :25,
                            'DVSN_NM'   :'Splash',
                            'DVSN_CD'   :3
                            },
                    'LS':{ 
                            'CPT'       :'LS',
                            'CNCPT_KEY' :31,
                            'DVSN_NM'   :'Lifestyle',
                            'DVSN_CD'   :5
                            },
                    'BS':{ 
                            'CPT'       :'BS',
                            'CNCPT_KEY' :28,
                            'DVSN_NM'   :'Babyshop',
                            'DVSN_CD'   :1
                            },
                    'IC':{ 
                            'CPT'       :'IC',
                            'CNCPT_KEY' :26,
                            'DVSN_NM'   :'Iconic',
                            'DVSN_CD'   :22
                            }
                }


    code_env_lookup = {}
    for num,item in enumerate(os.listdir(root+'Master_Ordering_Code')):
        code_env_lookup[str(num+1)] = item

    cod_str = 'Select the Code environment from the following list:'
    for item in list(code_env_lookup.keys()):
        temp_str = '\n'+item+'-'+code_env_lookup[item]
        cod_str = cod_str+temp_str

    chk = 0
    while chk == 0:

        try:
            CODE_ENV = str(raw_input(cod_str+'\n'))
        except:
            CODE_ENV = '0'
        if CODE_ENV == '0':
            print 'Invalid option selected, please select a valid option\n'
            print '-----------------------------------------------'
        else:
            chk = 1
            print '\n-----------------------------------------------'
            print 'Selected CODE_ENV is: ',code_env_lookup[CODE_ENV]
            print '-----------------------------------------------\n\n'
    del chk

    ver_env_lookup = {}
    for num,item in enumerate(sorted(os.listdir(root+'Master_Ordering_Code/'+code_env_lookup[CODE_ENV]))):
        ver_env_lookup[str(num+1)] = item


    cod_ver = 'Select the Code version from the following list:'
    for item in sorted(list(ver_env_lookup.keys())):
        temp_str = '\n'+item+'-'+ver_env_lookup[item]
        cod_ver = cod_ver+temp_str

    chk = 0
    while chk == 0:

        try:
            VERSION = str(raw_input(cod_ver+'\n'))
        except:
            VERSION = '0'
        if VERSION == '0':
            print 'Invalid option selected, please select a valid option\n'
            print '-----------------------------------------------'
        else:
            chk = 1
            print '\n-----------------------------------------------'
            print 'Selected VERSION is: ',ver_env_lookup[VERSION]
            print '-----------------------------------------------\n\n'
    del chk

    # choosing either to run the old forcast or new
    chk=0
    while chk==0:
        try:
            FcstMethod = raw_input('Select the Forcast Method\n1.Old\n2.New')
        except :
            FcstMethod =None
        if FcstMethod not in ['1','2']:
            print'Invalid option selected, please select a valid option\n'
            print'-----------------------------------------------'
        else:
            FcstMethod = 'Old' if FcstMethod==1 else 'New'
            print'\n-----------------------------------------------'
            print'Selected Forcast Method is:%s'%FcstMethod
            print'-----------------------------------------------\n\n'
            chk=1
    del chk





    #------------------------------------------------------------------
    #   Email - dict map

    mail_dict = {
            'VR':'Vinit.Rongata@landmarkgroup.com'
            ,'AM':'Abhishek.Meena@landmarkgroup.in'
            ,'ABS':'Abhijit.Sharma@landmarkgroup.com'
            ,'HR': 'harsh.ratde@landmarkgroup.in'
            ,'GR':'Gaurav.Ray@landmarkgroup.in'
            ,'MM':'Manois.Mathew@landmarkgroup.in'
            ,'DB':'Dimple.Bansal@landmarkgroup.in'
            ,'DH':'dipanjan.hait@landmarkgroup.in'
            ,'VS':'vaibhow.shukla@landmarkgroup.com'
            ,'SA':'Sahil.Aggarwal@landmarkgroup.com'
            ,'VIS':'Vishnu.Satheesh@landmarkgroup.com'
            ,'SJ':'shail.jain@landmarkgroup.com'
            ,'USP':'Udayshanker.Prakash@landmarkgroup.in'
            ,'NM':'Nitesh.Manhas@landmarkgroup.in'
            ,'TC':'Tanuj.Chakraborty@landmarkgroup.in'
            ,'SHR':'dipanjan.hait@landmarkgroup.in'
            ,'BU':'Bhagyashree.Upadhyay@landmarkgroup.in'
	    ,'MPA':'mithun.pakhira@landmarkgroup.in'

            }

    try:
        email_to = mail_dict[usr]
    except:
        email_to = 'Aman.Goswami@landmarkgroup.in'
        


    print '=================================================================\n'
    print '         PARAMETERS BEING USED FOR EXECUTING THE CODE'
    print '=================================================================\n'
    print 'USR \t\t:\t',usr
    print 'CNCPT \t\t:\t',cncpt
    print 'CPT \t\t:\t',ref_lookup[cncpt]['CPT']
    print 'DVSN_NM \t:\t',ref_lookup[cncpt]['DVSN_NM']
    print 'DVSN_CD \t:\t',ref_lookup[cncpt]['DVSN_CD']
    print 'CNCPT_KEY \t:\t',ref_lookup[cncpt]['CNCPT_KEY']
    print 'DEPT \t\t:\t',dept
    print 'TODAY \t\t:\t',today

    if cncpt == 'HC':
         print 'PREV_DAY \t\t:\t',prev_day

    print 'EXEC_ENV \t:\t',env
    print 'CODE_ENV \t:\t',code_env_lookup[CODE_ENV]
    print 'VERSION \t:\t',ver_env_lookup[VERSION]
    print 'INPUT_FILE \t:\t',dest_file_nm.split('.')[0]

    #r_code          =   'Rscript --no-save --no-restore --verbose '+ root +'Master_Ordering_Code/' +code_env_lookup[CODE_ENV] + '/'+ver_env_lookup[VERSION]+'/R_Code/Multivariate_Forecast_Module_Server.r  '+dep_path_loc+' '+dep_path_loc+'/Output/'+dept+'_BASE_MASTER.csv '+dep_path_loc+'/Output/'+dept+'_INDEP_HIST.csv '+dep_path_loc+'/Output/'+dept+'_INDEP_FRCST.csv > '+'/shared/sasdata02/ACOE_DEV/ordering_solution/ACoE_Ordering/Automation/automation_log/'+today+'/'+today+'_'+cncpt+'_'+dept+'_Order_execR.Rout 2>&1 '
    
    if cncpt == 'HC':
        try:
            list_fls = os.listdir(root+output_dest+'/'+env+'/'+prev_day+'/'+cncpt+'/'+dept+'/Input')
            distnct_lst=[]
            for item in list_fls:
                if '_HRHY_FCST_INPUT_' in item:
                    distnct_lst.append(item.split('_')[4])

            list_territory_hc = list(set(distnct_lst))
        except:
            list_territory_hc = ['AE','BH','OM','JD','RD','DM','KW','QT','EG','LB']

    #--------------- SAS command---------------------------------------
    if cncpt == 'HC':
        if prev_day == '':
            SAS_SPCL_CASE = ' '
        else:
            SAS_SPCL_CASE = '-set PREV_DAY '+prev_day+' '
    else:
        SAS_SPCL_CASE =' '

    #--------------- R command---------------------------------------

    #log directory structure
    log_root = '/shared/sasdata02/ACOE_DEV/ordering_solution/ACoE_Ordering/Automation/automation_log/'

    #create log structure
    #make_folder(log_root,today)
    make_folder(log_root,env+'/'+today+'/'+usr+'_'+cncpt+'_'+dept)
    log_path_loc = log_root+env+'/'+today+'/'+usr+'_'+cncpt+'_'+dept+'/'

    os.system('chmod -R 777 '+log_root+env+'/'+today)
    dep_path_loc = root+output_dest+'/'+env+'/'+today+'/'+cncpt+'/'+dept
    pre_forc_code   =   'nohup /shared/sasdata01/sas/grid0/sasconfig/Lev1/SASApp/BatchServer/sasbatch.sh -log '+log_path_loc+log_file_name1+' -print '+log_path_loc+lst_file_name1+' -batch -noterminal -sysin /shared/sasdata02/ACOE_DEV/ordering_solution/ACoE_Ordering/Automation/automate_main/Automated_Parameter_file1.sas -set CNCPT "'+cncpt+'" -set CPT "'+ref_lookup[cncpt]['CPT']+'" -set DVSN_NM "'+ref_lookup[cncpt]['DVSN_NM'] +'" -set CNCPT_KEY '+str(ref_lookup[cncpt]['CNCPT_KEY'])+' -set DVSN_CD '+str(ref_lookup[cncpt]['DVSN_CD'])+' -set USR "'+usr+'" -set DEPT "'+dept+'" -set EXEC_ENV "'+env+'" -set CODE_ENV "'+code_env_lookup[CODE_ENV]+'" -set VERSION "'+ver_env_lookup[VERSION]+'" -set TODAY '+today+' -set INPUT_FILE "'+dest_file_nm.split('.')[0]+'" -set CODE_FLOW_IND '+ str(1) +' -set EMAIL_TO "'+email_to+'" '+'-set ORD_TYP '+ORD_TYP+' '+SAS_SPCL_CASE
    post_forc_code  =   'nohup /shared/sasdata01/sas/grid0/sasconfig/Lev1/SASApp/BatchServer/sasbatch.sh -log '+log_path_loc+log_file_name2+' -print '+log_path_loc+lst_file_name2+' -batch -noterminal -sysin /shared/sasdata02/ACOE_DEV/ordering_solution/ACoE_Ordering/Automation/automate_main/Automated_Parameter_file1.sas -set CNCPT "'+cncpt+'" -set CPT "'+ref_lookup[cncpt]['CPT']+'" -set DVSN_NM "'+ref_lookup[cncpt]['DVSN_NM'] +'" -set CNCPT_KEY '+str(ref_lookup[cncpt]['CNCPT_KEY'])+' -set DVSN_CD '+str(ref_lookup[cncpt]['DVSN_CD'])+' -set USR "'+usr+'" -set DEPT "'+dept+'" -set EXEC_ENV "'+env+'" -set CODE_ENV "'+code_env_lookup[CODE_ENV]+'" -set VERSION "'+ver_env_lookup[VERSION]+'" -set TODAY '+today+' -set INPUT_FILE "'+dest_file_nm.split('.')[0]+'" -set CODE_FLOW_IND '+ str(2) +' -set EMAIL_TO "'+email_to+'" '+'-set ORD_TYP '+ORD_TYP+' '+SAS_SPCL_CASE

    r_code_all_cncpt         =   'Rscript --no-save --no-restore --verbose '+ root +'Master_Ordering_Code/' +code_env_lookup[CODE_ENV] + '/'+ver_env_lookup[VERSION]+'/R_Code/Multivariate_Forecast_Module_Server.r  '+dep_path_loc+' '+dep_path_loc+'/Output/'+dept+'_BASE_MASTER.csv '+dep_path_loc+'/Output/'+dept+'_INDEP_HIST.csv '+dep_path_loc+'/Output/'+dept+'_INDEP_FRCST.csv \> '+log_path_loc+today+'_'+cncpt+'_'+dept+'_Order_execR.Rout 2\>\&1 '
 
 
    print '\n-------------------------------------------------------------------'
    print 'Command to be execute for SAS_1 : '+pre_forc_code
    print 'Command to be execute for SAS_1 : '+post_forc_code
    print '-------------------------------------------------------------------\n'

    #print '\n-------------------------------------------------------------------'
    #print 'Command to be execute for R : '+r_code
    #print '-------------------------------------------------------------------\n\n'

    chk = 0
    while chk == 0:
        try:
            CODE_trig = str(raw_input('Please confirm to execute the above command:\n1-Execute\n2-Schedule\n3-Abort\n'))
        except:
            CODE_trig = '0'
        if CODE_trig not in ['1','2','3']:
            print 'Invalid option selected, please select a valid option\n'
            print '-----------------------------------------------'
        else:
            chk = 1
    del chk



    if CODE_trig in ['1','2']:

        #if schedule option is selected then the input for the delay of the code
        if CODE_trig == '2':
            schedule_chk = 0
            while schedule_chk == 0:
                schedule_trig = str(raw_input('Please enter the delay( in minutes) from now for executing the code:\n')).strip()
                print 'Requested delay for executing the code is: ',schedule_trig,'m\n'

                schedule_conf = str(raw_input('Please choose one of the following:\n1-Confirm\n2-want to change the delay\n')).strip()            
                while schedule_conf not in ['1','2']:
                    print 'Invalid option selected, please follow the instructions below\n'
                    schedule_conf = str(raw_input('Please choose one of the following:\n1-Confirm\n2-want to change the delay\n')).strip()

                if schedule_conf == '1':
                    schedule_chk = 1
        else:
            schedule_conf = '0'

        print 'Executing the command\n'
        print 'Log can be checked at: '+log_path_loc+log_file_name1
        print 'Log can be checked at: '+log_path_loc+log_file_name2

        shell_file_nm = log_path_loc+today+'_'+cncpt+'_'+dept+'_Order_exec.sh'
        # Delete the existing shell file in the destination        
        if today+'_'+cncpt+'_'+dept+'_Order_exec.sh' in os.listdir(log_path_loc):
            os.system('rm '+shell_file_nm)

        # Create the blank shell file
        os.system('touch '+shell_file_nm)

        #delay in the code
        if schedule_conf == '1':
            os.system('echo sleep '+schedule_trig+'m >> '+shell_file_nm)        

        # Write the script for SAS, to be executed before R code
        os.system('echo '+pre_forc_code+' >> '+shell_file_nm)

        # Write the script to be executed in R and post R
        if cncpt == 'HC':

            # R part 
            for item in list_territory_hc:
                r_code_HC = 'Rscript --no-save --no-restore --verbose '+ root +'Master_Ordering_Code/' +code_env_lookup[CODE_ENV] + '/'+ver_env_lookup[VERSION]+'/R_Code/Home_Center/HC_Hirarchical_Server.R  '+dep_path_loc+' '+today+' '+item+' \> '+log_path_loc+today+'_'+cncpt+'_'+dept+'_Order_execR_HRC_'+item+'.Rout 2\>\&1 '
                os.system('echo '+r_code_HC+' >> '+shell_file_nm)    
                r_code_HC = 'Rscript --no-save --no-restore --verbose '+ root +'Master_Ordering_Code/' +code_env_lookup[CODE_ENV] + '/'+ver_env_lookup[VERSION]+'/R_Code/Home_Center/HC_Non_Hirarchical_Server.R  '+dep_path_loc+' '+today+' '+item+' \> '+log_path_loc+today+'_'+cncpt+'_'+dept+'_Order_execR_NONHRC_'+item+'.Rout 2\>\&1 '
                os.system('echo '+r_code_HC+' >> '+shell_file_nm)    

            # Post R 

            # -------------------------------------------------------------------
            # PROMOUPLIFT:
            # -------------------------------------------------------------------

            #for item in list_territory_hc:
                post_R_HC = post_forc_code+'-set TERR_SELECTION '+item+' '
                os.system('echo '+post_R_HC+' >> '+shell_file_nm)            

        else:
            # R part
            os.system('echo '+r_code_all_cncpt+' >> '+shell_file_nm)

            # Post R 

            # -------------------------------------------------------------------
            # PROMOUPLIFT:
            # -------------------------------------------------------------------

            python_command = 'python2.7 '+root +'Master_Ordering_Code/' +code_env_lookup[CODE_ENV] + '/'+ver_env_lookup[VERSION]+'/3.Forecast_Adjustment/Python/Baseline_estimation_v1_Server.py ' +dep_path_loc+'/ '+'KEY_LVL' 
            os.system('echo '+python_command+' >> '+shell_file_nm)

            # -------------------------------------------------------------------                
            os.system('echo '+post_forc_code+' >> '+shell_file_nm)

            

        # Execute the shell file
        os.system('bash '+shell_file_nm+' &')

    elif CODE_trig == '3':
        sys.exit('Selected to abort the execution\n')

''''
file.close()

if file_copy_optn =='3':
    file = open('Automation_input_sheet.csv')
    
    for num,line in enumerate(file):
        if num == 0:
            header = line.replace('\n','')
        else:   
            lookup_dict = {}
            for i in range(0,len(header.split(','))):
                #print(i)
                lookup_dict[header.split(',')[i]]=line.replace('\n','').split(',')[i]
                #print(lookup_dict)
            
            #   create folder for hierarchy
            if lookup_dict['CNSDR_CASE'] == str(1):
                            
                try:
                    out_date = int(max([item for item in os.listdir(root+output_dest+'/'+env) if ((len(str(int(item)))==8) and (item != today) and (inputFileChk(root+output_dest+'/'+env+'/'+item+'/'+lookup_dict['CNCPT']+'/'+lookup_dict['dept']+'/'+'Input')!=[])) ]))        
                    
                    print('-------------------------------------------------------')    
                    print('Latest existing folder is {}. {} file will be copied for this run'.format(out_date,inputFileChk(root+output_dest+'/'+env+'/'+str(out_date)+'/'+lookup_dict['CNCPT']+'/'+lookup_dict['dept']+'/'+'Input')))
                    print('-------------------------------------------------------')
                    
                    #copying the identified file to the desination folder
                    os.system("cp {} {}".format(root+output_dest+'/'+env+'/'+str(out_date)+'/'+lookup_dict['CNCPT']+'/'+lookup_dict['dept']+'/'+'Input'+'/'+inputFileChk(root+output_dest+'/'+env+'/'+str(out_date)+'/'+lookup_dict['CNCPT']+'/'+lookup_dict['dept']+'/'+'Input')[0],root+output_dest+'/'+env+'/'+today+'/'+lookup_dict['CNCPT']+'/'+lookup_dict['dept']+'/'+'Input'))
                    
                except:
                    print('no other folder exist')
                    #file_copy_optn = '2'
                
                
                dest_folder = root+output_dest+'/'+env+'/'+today+'/'+lookup_dict['CNCPT']+'/'+lookup_dict['dept']+'/'+'Input'
                
                #rename
                source_file_nm = inputFileChk(dest_folder)[0]
                dest_file_nm = today+source_file_nm.split(source_file_nm.split('_')[0])[1] 
                os.system('mv {} {} '.format(dest_folder+'/'+source_file_nm,dest_folder+'/'+dest_file_nm))

                
    #----------------------------------------------------------------------------            
                make_folder(root,'Automation/automation_log/'+today)
                log_file_name = today+'_'+cncpt+'_'+dept+'_Order_exec.log'
                lst_file_name = today+'_'+cncpt+'_'+dept+'_Order_exec.lst'
                
                print('nohup /shared/sasdata01/sas/grid0/sasconfig/Lev1/SASApp/BatchServer/sasbatch.sh -log /shared/sasdata02/ACOE_DEV/ordering_solution/ACoE_Ordering/Automation/automation_log/'+today+'/'+log_file_name+' -print /shared/sasdata02/ACOE_DEV/ordering_solution/ACoE_Ordering/Automation/automation_log/'+today+'/'+lst_file_name+' -batch -noterminal -sysin /shared/sasdata02/ACOE_DEV/ordering_solution/ACoE_Ordering/Automation/automate_main/Automated_Parameter_file1.sas -set CNCPT "'+cncpt+'" -set CPT "'+lookup_dict['CPT']+'" -set DVSN_NM "'+lookup_dict['dvsn_nm'] +'" -set CNCPT_KEY '+lookup_dict['cncpt_key']+' -set DVSN_CD '+lookup_dict['dvsn_cd']+' -set USR "'+lookup_dict['usr']+'" -set DEPT "'+lookup_dict['dept']+'" -set EXEC_ENV "'+lookup_dict['EXEC_ENV']+'" -set CODE_ENV "'+lookup_dict['CODE_ENV']+'" -set VERSION "'+lookup_dict['VERSION']+'" -set TODAY '+today+' -set INPUT_FILE "'+dest_file_nm+'" &')
                time.sleep(60)
                print('\n')
                '''
