# -*- coding:utf-8 -*- 
# 2018-07-04 17:26:47
# author: shaocong_mo@163.com

import os, time, subprocess, re
from sys import argv

class ErlCompile():
    """docstring for ErlCompile"""
    def __init__(self, base_path, src_pathes, output_path, include_pathes, change_time, erl):
        self.base_path = base_path
        self.src_pathes = src_pathes
        self.output_path = output_path
        self.include_pathes = include_pathes
        self.change_time = change_time
        self.erl = erl

        self.file_type = '.erl'
        self.emakefile = 'Emakefile'

    def __get_update_mod__(self):
        now = time.time()
        mods = list()
        for src_path in self.src_pathes:
            for root, dirs, files in os.walk(src_path):
                for file in files:
                    file_path = root + '/' + file
                    if os.path.isfile(file_path) and file_path.endswith(self.file_type):
                        file_info = os.stat(file_path)
                        if file_info.st_mtime > now - self.change_time and file_info.st_mtime < now + self.change_time:
                            mods.append(file_path.replace(src_path + '/', ''))
        return mods

    def __create_emakefile__(self, mods):
        params = list()
        for include_path in self.include_pathes:
            params.append('{i, "' + include_path + '"}')
        params.append('{outdir,"' + self.output_path + '"}')
        data = '{' + str(mods).replace('\'', '"') + ',' + str(params).replace('\'', '') + '}.'
        emakefile_path = self.base_path + '/' + self.emakefile
        with open(emakefile_path, 'w') as emakefile:
            emakefile.write(data)

    def __make__(self):
        subprocess.call(self.erl + ' -make')

    def compile(self):
        mods = self.__get_update_mod__()
        if len(mods) > 0:
            self.__create_emakefile__(mods)
            self.__make__()

if __name__ == '__main__':
    # sublime project conf sample
    # "shell_cmd": "python -u $project_path/app/ErlCompile.py $project_path/app 1 $project_path/app $project_path/ebin 1 $project_path/hrl 300 d:/erl5.10.4/bin/erl",
    index = 1
    path = argv[index]
    src_pathes = list()
    index += 1
    for i in range(int(argv[index])):
        index += 1
        src_pathes.append(argv[index])
    index += 1
    output_path = argv[index]
    index += 1
    include_pathes = list()
    for j in range(int(argv[index])):
        index += 1
        include_pathes.append(argv[index])
    index += 1
    change_time = int(argv[index])
    index += 1
    erl_path = argv[index]
    erl_compile = ErlCompile(path, src_pathes, output_path, include_pathes, change_time, erl_path)
    erl_compile.compile()
