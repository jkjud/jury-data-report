import { Sankey } from "./Sankey";
import { juryData } from "./data";

export const App = () => {
  const handleNavigate = (target: string) => {
    console.log("navigate ->", target);
    alert(`Navigate to: ${target}`);
  };

  return (
    <div className="page">
      <h2 className="title">Jury Pipeline (nivo)</h2>
      <div className="sankey-wrap">
        <Sankey data={juryData} onNavigate={handleNavigate} />
      </div>
    </div>
  );
};
