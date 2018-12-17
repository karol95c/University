using System.Collections;
using System.Collections.Generic;
using UnityEngine;
public class BulletScript : MonoBehaviour {

	// Use this for initialization
	public GameObject brickExplodePrefab;
	public Transform brickExplodeSpawn;
	static GameObject mapGO;
	string collisionTag;
	void Start () {
		mapGO = GameObject.Find("Map");
	}
	
	// Update is called once per frame
	void Update () {

	}

	void OnCollisionEnter(Collision col)
	{
		collisionTag = col.gameObject.tag;
		if (collisionTag != "Water" || collisionTag != "Ivy")
		{
			Destroy(this.gameObject);
		}
		if (collisionTag == "Brick")
		{

			col.gameObject.GetComponent<BrickScript>().Explode();
			
		}
		else if (collisionTag == "EnemyTank")
		{
			EnemyTankScript enemyTankScript = col.gameObject.GetComponent<EnemyTankScript>();
			enemyTankScript.Explode();
			EnemyTankScript.setNextID(enemyTankScript.getID());
			mapGO.GetComponent<MapScript>().createEnemyTank();
		}
		else if(collisionTag == "Player")
		{
			col.gameObject.GetComponent<PlayerTankScript>().gotHit() ;
		}
		else if (collisionTag == "HeartBase")
		{
			Destroy(col.gameObject);
			GameObject.Find("GameManager").GetComponent<GameManagerSc>().gameOver();
		}
		
	}
}
