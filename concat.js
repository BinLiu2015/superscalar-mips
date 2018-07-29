const fs = require('fs').promises

async function concat() {
    const files = await fs.readdir(`${process.env.PWD}/mipssingle`)
    const newFile = await fs.open(`${process.env.PWD}/mips-superscalar`, 'w')
    files.forEach(async (file) => {
        const f = await fs.open(`${process.env.PWD}/mipssingle/${file}`, 'r')

        const content = await f.readFile()

        await newFile.appendFile(mountString(content, file))
    })
}

function mountString(buffer, fileName) {
    const header = `-- ==========================================================\n-- ${fileName}\n\n`
    return `${header}${buffer.toString()}\n\n`
}

concat()