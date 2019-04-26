#!/usr/bin/env python
# -*- coding: UTF-8 -*-
import sys
import xlrd
import os
import os.path
 
def gen_data(filename):
 wb = xlrd.open_workbook('excle/' + filename + '.xlsx') # 加载文件所有分页
 
# sheetnames = wb.get_sheet_names() # 获取所有分页的名字列表
 ws = wb.sheet_by_index(0) # 取第一个分页的数据
# print 'ws:', ws
 
# print "Work Sheet Titile:", ws.title # 分页名称
# print "Work Sheet Rows:", ws.max_row # 分页行数
# print "Work Sheet Cols:", ws.max_column # 分页列数
 
 content = [] # 数据内容
 id_list = [] # ID列表
 
# ========================start concat need data=================
 content.append('%% this file is auto maked!\n')
 content.append('-module(' + filename + ').\n')
 content.append('-compile(export_all).\n')
 content.append('-include(data.hrl).\n')
 
 ServerCol = []  # 需要转换服务端数据的列
 for j in range(ws.ncols):
     if ws.cell(1, j).value == "server":
        ServerCol.append(j)
 #print(ServerCol)
 for i in range(2, ws.nrows):  # 从表格第二行开始读取
  #print(ws.row_values(i))
  if ws.row_values(i) == []:   # 最后一行结束循环
      break
  for j in ServerCol:
    value = ws.cell(i,j).value
    ctype= ws.cell(i,j).ctype
    if ctype == 2 and value % 1 == 0:  # 如果是整形
        value = int(value)
    if value == "":
        content.append(' ,""')
    elif j == 0:
     id_list.append(value)
     content.append('get(' + str(value).strip() + ') ->\n')
     content.append(' {' + filename + ', ' + str(value).strip())
    else:
     content.append(' ,' + str(value).strip())
  content.append('};\n')
 
 content.append('get(_) ->\n')
 content.append(' not_match.\n')
 
 content.append('length() ->\n')
 content.append(' ' + str(ws.nrows - 1) + '.\n')
 content.append('id_list() ->\n ' + str(id_list) + '.')
# ==============================end===========================
 # 写入数据
 f = file('./game_data/' + filename + '.erl','w+')
 f.writelines(content)
 print 'create new file:', filename + '.erl'
 f.close() # 关闭通道
 return
 
def start_gen():
 # 删除旧的数据
 delnames = os.listdir('./game_data')
 for delname in delnames:
    os.remove('./game_data/' + delname)
    print 'delete old file:', delname
 filenames = os.listdir('./excle')
 for filename in filenames: # 遍历文件
   find = filename.find('.xlsx') # 返回该文件名称长度
   #print "find is:", find
   if filename[0] == '~' or find == -1: # 文件名以'~'开头或者找不到文件名， 如以'.'开头的文件
    continue
   else:
    split_list = filename.split('.') # 使用'.'分割文件名，获得[文件名,文件格式]
#    print split_list
    gen_data(split_list[0]) # 用文件名作为参数调用gen_data
 
start_gen()

