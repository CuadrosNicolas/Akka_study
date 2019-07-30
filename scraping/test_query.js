const {queryChain} = require("./QueryChain")
let start = async function () {
	/**
	 * Get repositories containing the akka keywords
	 * Written in Scala
	 * Push between 2019-06-01 and 2019-06-02
	 * With an amount of stars superior to 0
	 * With a step of 1 day between each sub request of repository
	 */
	const chainName = "AkkaProjects"
			queryChain(chainName, {
				type : "query",
					query : {
					keywords: "",
					language: "scala",
					begin: new Date("2019-01-01"),
					end: new Date("2019-07-01"),
					stars: ">0",
					step: 1
					}
				})
				.checkFile({
					keywords : "akka",
					fileName:"build",
					fileExtension:"sbt"
				},"sbt")
				//.checkProperty(r=>true)//r.properties.actor.valid && r.properties.test.valid)
				.clone("./results")	//Clone the repository
									//Add the fullPath property to the repository
				 .checkCommand((r) =>
				 `cd ${r.properties.fullPath} && sbt compile < /dev/null;`
				 ,"buildable")	//Test to compile

				.run((r)=>{
					const fs = require("fs")
					let temp =r
					let tab = Object.keys(temp).map(key => {
						return key + " " + temp[key].properties.fullPath + " " + temp[key].name + " " + temp[key].html_url
					}).join("\n");
					fs.writeFileSync("../akka_process/list.csv", tab)
				}) //Shows each repositories that fulfill all criterias

}
start()