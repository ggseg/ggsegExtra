#!/bin/bash -x

export TD=$(mktemp -d)
trap 'rm -rf $TD; exit 0' 0 1 2 3 14 15


export LABFILE=$1
export SURFFILE=${2}
export RESULT=${3}
wb_command -label-export-table ${LABFILE} ${TD}/a
sed -n '0~2!p' ${TD}/a > ${TD}/names

function Smooth1()
{
    i=${1}
    METRICNAME=${TD}/${i}.func.gii  
    echo $i $METRICNAME
    wb_command -gifti-label-to-roi ${LABFILE} ${METRICNAME} -name ${i}
    wb_command -metric-erode ${METRICNAME} ${SURFFILE} 2  ${METRICNAME}
    wb_command -metric-remove-islands ${SURFFILE} ${METRICNAME} ${METRICNAME}
    wb_command -metric-dilate ${METRICNAME} ${SURFFILE} 2  ${METRICNAME}

}

module load gnuparallel/20190122

export -f Smooth1

cat ${TD}/names | parallel Smooth1 

m=$(cat ${TD}/names | parallel echo -n '\ -metric ${TD}/{}.func.gii\ ')
wb_command -metric-merge ${TD}/multicol.func.gii ${m}

wb_command -metric-reduce ${TD}/multicol.func.gii MAX ${TD}/smooth.func.gii
wb_command -label-mask ${LABFILE} ${TD}/smooth.func.gii  ${TD}/smooth.label.gii

wb_command -label-dilate ${TD}/smooth.label.gii ${SURFFILE} 5  ${TD}/dilate.label.gii
wb_command -label-erode ${TD}/dilate.label.gii ${SURFFILE} 5 ${RESULT}
